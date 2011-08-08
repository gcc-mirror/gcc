// go.cc -- Go frontend main file for gcc.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "go-c.h"

#include "lex.h"
#include "parse.h"
#include "backend.h"
#include "gogo.h"

// The unique prefix to use for exported symbols.  This is set during
// option processing.

static std::string unique_prefix;

// The data structures we build to represent the file.
static Gogo* gogo;

// Create the main IR data structure.

GO_EXTERN_C
void
go_create_gogo(int int_type_size, int pointer_size)
{
  go_assert(::gogo == NULL);
  ::gogo = new Gogo(go_get_backend(), int_type_size, pointer_size);
  if (!unique_prefix.empty())
    ::gogo->set_unique_prefix(unique_prefix);

  // FIXME: This should be in the gcc dependent code.
  ::gogo->define_builtin_function_trees();
}

// Set the unique prefix we use for exported symbols.

GO_EXTERN_C
void
go_set_prefix(const char* arg)
{
  unique_prefix = arg;
  for (size_t i = 0; i < unique_prefix.length(); ++i)
    {
      char c = unique_prefix[i];
      if ((c >= 'a' && c <= 'z')
	  || (c >= 'A' && c <= 'Z')
	  || (c >= '0' && c <= '9')
	  || c == '_')
	;
      else
	unique_prefix[i] = '_';
    }
}

// Parse the input files.

GO_EXTERN_C
void
go_parse_input_files(const char** filenames, unsigned int filename_count,
		     bool only_check_syntax, bool require_return_statement)
{
  go_assert(filename_count > 0);
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
	    fatal_error("cannot open %s: %m", filename);
	}

      Lex lexer(filename, file);

      Parse parse(&lexer, ::gogo);
      parse.program();

      if (strcmp(filename, "-") != 0)
	fclose(file);
    }

  ::gogo->clear_file_scope();

  // If the global predeclared names are referenced but not defined,
  // define them now.
  ::gogo->define_global_names();

  // Finalize method lists and build stub methods for named types.
  ::gogo->finalize_methods();

  // Now that we have seen all the names, lower the parse tree into a
  // form which is easier to use.
  ::gogo->lower_parse_tree();

  // Now that we have seen all the names, verify that types are
  // correct.
  ::gogo->verify_types();

  // Work out types of unspecified constants and variables.
  ::gogo->determine_types();

  // Check types and issue errors as appropriate.
  ::gogo->check_types();

  if (only_check_syntax)
    return;

  // Check that functions have return statements.
  if (require_return_statement)
    ::gogo->check_return_statements();

  // Export global identifiers as appropriate.
  ::gogo->do_exports();

  // Turn short-cut operators (&&, ||) into explicit if statements.
  ::gogo->remove_shortcuts();

  // Use temporary variables to force order of evaluation.
  ::gogo->order_evaluations();

  // Build thunks for functions which call recover.
  ::gogo->build_recover_thunks();

  // Convert complicated go and defer statements into simpler ones.
  ::gogo->simplify_thunk_statements();
  
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
