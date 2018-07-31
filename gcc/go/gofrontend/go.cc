// go.cc -- Go frontend main file for gcc.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "go-c.h"
#include "go-diagnostics.h"

#include "lex.h"
#include "parse.h"
#include "backend.h"
#include "gogo.h"

// The data structures we build to represent the file.
static Gogo* gogo;

// Create the main IR data structure.

GO_EXTERN_C
void
go_create_gogo(const struct go_create_gogo_args* args)
{
  go_assert(::gogo == NULL);
  ::gogo = new Gogo(args->backend, args->linemap, args->int_type_size,
		    args->pointer_size);

  if (args->pkgpath != NULL)
    ::gogo->set_pkgpath(args->pkgpath);
  else if (args->prefix != NULL)
    ::gogo->set_prefix(args->prefix);

  if (args->relative_import_path != NULL)
    ::gogo->set_relative_import_path(args->relative_import_path);
  ::gogo->set_check_divide_by_zero(args->check_divide_by_zero);
  ::gogo->set_check_divide_overflow(args->check_divide_overflow);
  if (args->compiling_runtime)
    ::gogo->set_compiling_runtime(args->compiling_runtime);
  if (args->c_header != NULL)
    ::gogo->set_c_header(args->c_header);
  ::gogo->set_debug_escape_level(args->debug_escape_level);
  if (args->debug_escape_hash != NULL)
    ::gogo->set_debug_escape_hash(args->debug_escape_hash);
  ::gogo->set_nil_check_size_threshold(args->nil_check_size_threshold);
}

// Parse the input files.

GO_EXTERN_C
void
go_parse_input_files(const char** filenames, unsigned int filename_count,
		     bool only_check_syntax, bool)
{
  go_assert(filename_count > 0);

  Lex::Linknames all_linknames;
  for (unsigned int i = 0; i < filename_count; ++i)
    {
      if (i > 0)
	::gogo->clear_file_scope();

      const char* filename = filenames[i];
      FILE* file;
      if (strcmp(filename, "-") == 0)
	file = stdin;
      else
	{
	  file = fopen(filename, "r");
	  if (file == NULL)
	    go_fatal_error(Linemap::unknown_location(),
			   "cannot open %s: %m", filename);
	}

      Lex lexer(filename, file, ::gogo->linemap());

      Parse parse(&lexer, ::gogo);
      parse.program();

      if (strcmp(filename, "-") != 0)
	fclose(file);

      Lex::Linknames* linknames = lexer.get_and_clear_linknames();
      if (linknames != NULL)
	{
	  if (!::gogo->current_file_imported_unsafe())
	    {
	      for (Lex::Linknames::const_iterator p = linknames->begin();
		   p != linknames->end();
		   ++p)
		go_error_at(p->second.loc,
			    ("//go:linkname only allowed in Go files that "
			     "import \"unsafe\""));
	    }
	  all_linknames.insert(linknames->begin(), linknames->end());
	}
    }

  ::gogo->linemap()->stop();

  ::gogo->clear_file_scope();

  // If the global predeclared names are referenced but not defined,
  // define them now.
  ::gogo->define_global_names();

  // Apply any go:linkname directives.
  for (Lex::Linknames::const_iterator p = all_linknames.begin();
       p != all_linknames.end();
       ++p)
    ::gogo->add_linkname(p->first, p->second.is_exported, p->second.ext_name,
			 p->second.loc);

  // Finalize method lists and build stub methods for named types.
  ::gogo->finalize_methods();

  // Check that functions have a terminating statement.
  ::gogo->check_return_statements();

  // Now that we have seen all the names, lower the parse tree into a
  // form which is easier to use.
  ::gogo->lower_parse_tree();

  // Create function descriptors as needed.
  ::gogo->create_function_descriptors();

  // Now that we have seen all the names, verify that types are
  // correct.
  ::gogo->verify_types();

  // Work out types of unspecified constants and variables.
  ::gogo->determine_types();

  // Check types and issue errors as appropriate.
  ::gogo->check_types();

  if (only_check_syntax)
    return;

  ::gogo->analyze_escape();

  // Export global identifiers as appropriate.
  ::gogo->do_exports();

  // Use temporary variables to force order of evaluation.
  ::gogo->order_evaluations();

  // Turn short-cut operators (&&, ||) into explicit if statements.
  ::gogo->remove_shortcuts();

  // Convert named types to backend representation.
  ::gogo->convert_named_types();

  // Build thunks for functions which call recover.
  ::gogo->build_recover_thunks();

  // Convert complicated go and defer statements into simpler ones.
  ::gogo->simplify_thunk_statements();

  // Write out queued up functions for hash and comparison of types.
  ::gogo->write_specific_type_functions();

  // Add write barriers.
  ::gogo->add_write_barriers();

  // Flatten the parse tree.
  ::gogo->flatten();

  // Reclaim memory of escape analysis Nodes.
  ::gogo->reclaim_escape_nodes();

  // Dump ast, use filename[0] as the base name
  ::gogo->dump_ast(filenames[0]);
}

// Write out globals.

GO_EXTERN_C
void
go_write_globals()
{
  return ::gogo->write_globals();
}

// Return the global IR structure.  This is used by some of the
// langhooks to pass to other code.

Gogo*
go_get_gogo()
{
  return ::gogo;
}
