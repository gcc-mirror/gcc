// rust.cc - Rust frontend main file for GCC

// copyright, etc.

#include "rust-system.h"

#include "rust-c.h"
#include "rust-diagnostics.h"

#include "rs-backend.h"
#include "rustrust.h"
#include "rs-lex.h"
#include "rs-parse.h"

// The data structures we build to represent the file.
static Gorust* rustrust;
// replace with rdot or something else?

// Create the main IR data structure.
RUST_EXTERN_C void rust_create_rustrust(const struct rust_create_rustrust_args* args) {
    rust_assert(::rustrust == NULL);
    ::rustrust = new Gorust(args->backend, args->linemap, args->int_type_size, args->pointer_size);

    if (args->pkgpath != NULL)
        ::rustrust->set_pkgpath(args->pkgpath);
    else if (args->prefix != NULL)
        ::rustrust->set_prefix(args->prefix);

    if (args->relative_import_path != NULL)
        ::rustrust->set_relative_import_path(args->relative_import_path);
    ::rustrust->set_check_divide_by_zero(args->check_divide_by_zero);
    ::rustrust->set_check_divide_overflow(args->check_divide_overflow);
    if (args->compiling_runtime)
        ::rustrust->set_compiling_runtime(args->compiling_runtime);
    if (args->c_header != NULL)
        ::rustrust->set_c_header(args->c_header);
    ::rustrust->set_debug_escape_level(args->debug_escape_level);
    if (args->debug_escape_hash != NULL)
        ::rustrust->set_debug_escape_hash(args->debug_escape_hash);
    ::rustrust->set_nil_check_size_threshold(args->nil_check_size_threshold);
    if (args->debug_optimization)
        ::rustrust->set_debug_optimization(args->debug_optimization);
}

// Parse the input files.
RUST_EXTERN_C void grs_parse_input_files(const char** filenames, unsigned int filename_count, 
    bool only_check_syntax, bool) {
    rust_assert(filename_count > 0);

    Lex::Linknames all_linknames;
    for (unsigned int i = 0; i < filename_count; ++i) {
        if (i > 0)
            ::rustrust->clear_file_scope();

        const char* filename = filenames[i];
        FILE* file;
        if (strcmp(filename, "-") == 0)
            file = stdin;
        else {
            file = fopen(filename, "r");
            if (file == NULL)
                rust_fatal_error(Linemap::unknown_location(),
                               "cannot open %s: %m",
                               filename);
        }

        Lex lexer(filename, file, ::rustrust->linemap());

        Parse parse(&lexer, ::rustrust);
        parse.program();

        if (strcmp(filename, "-") != 0)
            fclose(file);

        Lex::Linknames* linknames = lexer.get_and_clear_linknames();
        if (linknames != NULL) {
            if (!::rustrust->current_file_imported_unsafe()) {
                for (Lex::Linknames::const_iterator p = linknames->begin();
                     p != linknames->end();
                     ++p)
                    rust_error_at(p->second.loc,
                                ("//rust:linkname only allowed in Go files that "
                                 "import \"unsafe\""));
            }
            all_linknames.insert(linknames->begin(), linknames->end());
        }
    }

    ::rustrust->clear_file_scope();

    // If the global predeclared names are referenced but not defined,
    // define them now.
    ::rustrust->define_global_names();

    // Apply any rust:linkname directives.
    for (Lex::Linknames::const_iterator p = all_linknames.begin();
         p != all_linknames.end();
         ++p)
        ::rustrust->add_linkname(p->first, p->second.is_exported, p->second.ext_name, p->second.loc);

    // Finalize method lists and build stub methods for named types.
    ::rustrust->finalize_methods();

    // Check that functions have a terminating statement.
    ::rustrust->check_return_statements();

    // Now that we have seen all the names, lower the parse tree into a
    // form which is easier to use.
    ::rustrust->lower_parse_tree();

    // At this point we have handled all inline functions, so we no
    // longer need the linemap.
    ::rustrust->linemap()->stop();

    // Create function descriptors as needed.
    ::rustrust->create_function_descriptors();

    // Now that we have seen all the names, verify that types are
    // correct.
    ::rustrust->verify_types();

    // Work out types of unspecified constants and variables.
    ::rustrust->determine_types();

    // Check types and issue errors as appropriate.
    ::rustrust->check_types();

    if (only_check_syntax)
        return;

    // Make implicit type conversions explicit.
    ::rustrust->add_conversions();

    // Analyze the program flow for escape information.
    ::rustrust->analyze_escape();

    // Export global identifiers as appropriate.
    ::rustrust->do_exports();

    // Use temporary variables to force order of evaluation.
    ::rustrust->order_evaluations();

    // Turn short-cut operators (&&, ||) into explicit if statements.
    ::rustrust->remove_shortcuts();

    // Convert named types to backend representation.
    ::rustrust->convert_named_types();

    // Build thunks for functions which call recover.
    ::rustrust->build_recover_thunks();

    // Convert complicated rust and defer statements into simpler ones.
    ::rustrust->simplify_thunk_statements();

    // Write out queued up functions for hash and comparison of types.
    ::rustrust->write_specific_type_functions();

    // Add write barriers.
    ::rustrust->add_write_barriers();

    // Flatten the parse tree.
    ::rustrust->flatten();

    // Reclaim memory of escape analysis Nodes.
    ::rustrust->reclaim_escape_nodes();

    // Dump ast, use filename[0] as the base name
    ::rustrust->dump_ast(filenames[0]);
}

// Write out globals.
RUST_EXTERN_C void rust_write_globals() {
    return ::rustrust->write_globals();
}

// Return the global IR structure.  This is used by some of the langhooks to pass to other code.
Rustrust* rust_get_rustrust() {
    return ::rustrust;
}