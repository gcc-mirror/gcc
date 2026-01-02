/* A pure C API for emitting diagnostics.
   Copyright (C) 2023-2026 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef LIBGDIAGNOSTICS_H
#define LIBGDIAGNOSTICS_H

#include <stdarg.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/**********************************************************************
 Compatibility macros.
 **********************************************************************/

/* This macro simplifies testing whether we are using gcc, and if it
   is of a particular minimum version. (Both major & minor numbers are
   significant.)  This macro will evaluate to 0 if we are not using
   gcc at all.  */
#define LIBGDIAGNOSTICS_GCC_VERSION (__GNUC__ * 1000 + __GNUC_MINOR__)

/**********************************************************************
 Macros for attributes.
 **********************************************************************/

# if (LIBGDIAGNOSTICS_GCC_VERSION >= 3003)
#  define LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL(ARG_NUM) __attribute__ ((__nonnull__ (ARG_NUM)))
# else
#  define LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL(ARG_NUM)
# endif /* GNUC >= 3.3 */

#define LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL(ARG_NUM)
  /* empty; for the human reader */

# if (LIBGDIAGNOSTICS_GCC_VERSION >= 4001)
#  define LIBGDIAGNOSTICS_PARAM_FORMAT_STRING(FMT_KIND, FMT_ARG_NUM, ARGS_ARG_NUM) \
     __attribute__ ((__format__ (FMT_KIND, FMT_ARG_NUM, ARGS_ARG_NUM)))
# else
#  define LIBGDIAGNOSTICS_PARAM_FORMAT_STRING(FMT_KIND, FMT_ARG_NUM, ARGS_ARG_NUM)
# endif /* GNUC >= 4.1 */

#define LIBGDIAGNOSTICS_PARAM_GCC_FORMAT_STRING(FMT_ARG_NUM, ARGS_ARG_NUM) \
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (FMT_ARG_NUM)
  /* In theory we'd also add
       __attribute__ ((__format__ (__gcc_diag__, FMT_ARG_NUM, ARGS_ARG_NUM)))
     if LIBGDIAGNOSTICS_GCC_VERSION >= 4001
     However, doing so leads to warnings from -Wformat-diag, which is part
     of -Wall but undocumented, and much fussier than I'd want to inflict
     on users of libgdiagnostics.  */

#define LIBGDIAGNOSTICS_PARAM_PRINTF_FORMAT_STRING(FMT_ARG_NUM, ARGS_ARG_NUM) \
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (FMT_ARG_NUM) \
  LIBGDIAGNOSTICS_PARAM_FORMAT_STRING(gnu_printf, FMT_ARG_NUM, ARGS_ARG_NUM)

/**********************************************************************
 Data structures and types.
 All structs within the API are opaque.
 **********************************************************************/

/* An opaque bundle of state for a client of the library.
   Has zero of more "sinks" to which diagnostics are emitted.
   Responsibilities:
   - location-management
   - caching of source file content
   - patch generation.  */
typedef struct diagnostic_manager diagnostic_manager;

/* Types relating to diagnostic output sinks.  */

typedef struct diagnostic_text_sink diagnostic_text_sink;

/* An enum for determining if we should colorize a text output sink.  */
enum diagnostic_colorize
{
  DIAGNOSTIC_COLORIZE_IF_TTY,
  DIAGNOSTIC_COLORIZE_NO,
  DIAGNOSTIC_COLORIZE_YES
};

/* An enum for choosing the SARIF version for a SARIF output sink.  */

enum diagnostic_sarif_version
{
  DIAGNOSTIC_SARIF_VERSION_2_1_0,
  DIAGNOSTIC_SARIF_VERSION_2_2_PRERELEASE
};

/* Types relating to "physical" source locations i.e. locations within
   specific files expressed via line/column.  */

/* Opaque type describing a particular input file.  */
typedef struct diagnostic_file diagnostic_file;

/* Opaque type representing a key into a database of source locations within
   a diagnostic_manager.  Locations are created by various API calls into
   the diagnostic_manager expressing source code points and ranges.  They
   persist until the diagnostic_manager is released, which cleans them
   up.

   NULL means "UNKNOWN", and can be returned by the manager as a
   fallback when a problem occurs (e.g. too many locations).

   A diagnostic_location can be a single point within the source code,
   such as here (at the the '"' at the start of the string literal):

   |  int i = "foo";
   |          ^

   or be a range with a start and finish, and a "caret" location.

   |   a = (foo && bar)
   |       ~~~~~^~~~~~~

   where the caret here is at the first "&", and the start and finish
   are at the parentheses.  */

typedef struct diagnostic_physical_location diagnostic_physical_location;

/* Types for storing line and column information in text files.

   Both libgdiagnostics and emacs number source *lines* starting at 1, but
   they have differing conventions for *columns*.

   libgdiagnostics uses a 1-based convention for source columns,
   whereas Emacs's M-x column-number-mode uses a 0-based convention.

   For example, an error in the initial, left-hand
   column of source line 3 is reported by libgdiagnostics as:

      some-file.c:3:1: error: ...etc...

   On navigating to the location of that error in Emacs
   (e.g. via "next-error"),
   the locus is reported in the Mode Line
   (assuming M-x column-number-mode) as:

     some-file.c   10%   (3, 0)

   i.e. "3:1:" in libgdiagnostics corresponds to "(3, 0)" in Emacs.  */

typedef unsigned int diagnostic_line_num_t;
typedef unsigned int diagnostic_column_num_t;

/* An opaque type describing a "logical" source location
   e.g. "within function 'foo'".  */

typedef struct diagnostic_logical_location diagnostic_logical_location;

/* An enum for discriminating between different kinds of logical location
   for a diagnostic.

   Roughly corresponds to logicalLocation's "kind" property in SARIF v2.1.0
   (section 3.33.7).  */

enum diagnostic_logical_location_kind_t
{
 /* Kinds within executable code.  */
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_FUNCTION,
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_MEMBER,
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_MODULE,
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_NAMESPACE,
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_TYPE,
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_RETURN_TYPE,
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_PARAMETER,
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_VARIABLE,

  /* Kinds within XML or HTML documents.  */
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_ELEMENT,
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_ATTRIBUTE,
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_TEXT,
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_COMMENT,
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_PROCESSING_INSTRUCTION,
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_DTD,
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_DECLARATION,

  /* Kinds within JSON documents.  */
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_OBJECT,
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_ARRAY,
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_PROPERTY,
  DIAGNOSTIC_LOGICAL_LOCATION_KIND_VALUE
};

/* A "diagnostic" is an opaque bundle of state for a particular
   diagnostic that is being constructed in memory.

   A diagnostic has a primary location and zero or more secondary
   locations.  For example:

   |   a = (foo && bar)
   |       ~~~~~^~~~~~~

   This diagnostic has a single diagnostic_location, with the caret
   at the first "&", and the start/finish at the parentheses.

   Contrast with:

   |   a = (foo && bar)
   |        ~~~ ^~ ~~~

   This diagnostic has three locations
   - The primary location (at "&&") has its caret and start location at
   the first "&" and end at the second "&.
   - The secondary location for "foo" has its start and finish at the "f"
   and "o" of "foo"; the caret is not flagged for display, but is perhaps at
   the "f" of "foo".
   - Similarly, the other secondary location (for "bar") has its start and
   finish at the "b" and "r" of "bar"; the caret is not flagged for
   display, but is perhaps at the"b" of "bar".  */
typedef struct diagnostic diagnostic;

enum diagnostic_level
{
  DIAGNOSTIC_LEVEL_ERROR,
  DIAGNOSTIC_LEVEL_WARNING,
  DIAGNOSTIC_LEVEL_NOTE,

  /* A problem where the input is valid, but the tool isn't
     able to handle it.  */
  DIAGNOSTIC_LEVEL_SORRY
};

/* Types for working with execution paths.  */
typedef struct diagnostic_execution_path diagnostic_execution_path;
typedef int diagnostic_event_id;

typedef struct diagnostic_message_buffer diagnostic_message_buffer;

/**********************************************************************
 API entrypoints.
 **********************************************************************/

/* Create a new diagnostic_manager.
   The client needs to call diagnostic_release_manager on it at some
   point.
   Note that no output sinks are created by default.  */

extern diagnostic_manager *
diagnostic_manager_new (void);

/* Release a diagnostic_manager.
   This will flush output to all of the output sinks, and clean up. */

extern void
diagnostic_manager_release (diagnostic_manager *)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Optional metadata about the manager.  */

/* Set a string suitable for use as the value of the SARIF "name" property
   (SARIF v2.1.0 section 3.19.8).  */

extern void
diagnostic_manager_set_tool_name (diagnostic_manager *diag_mgr,
				  const char *value)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Set a string suitable for use as the value of the SARIF "fullName" property
   (SARIF v2.1.0 section 3.19.9).  */

extern void
diagnostic_manager_set_full_name (diagnostic_manager *diag_mgr,
				  const char *value)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Set a string suitable for use as the value of the SARIF "version" property
   (SARIF v2.1.0 section 3.19.13).  */

extern void
diagnostic_manager_set_version_string (diagnostic_manager *diag_mgr,
				       const char *value)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Set a string suitable for use as the value of the SARIF "informationUri"
   property (SARIF v2.1.0 section 3.19.17).  */

extern void
diagnostic_manager_set_version_url (diagnostic_manager *diag_mgr,
				    const char *value)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Destinations for diagnostics.  */

/* Add a new output sink to DIAG_MGR, which writes GCC-style diagnostics
   to DST_STREAM.
   Return a borrowed pointer to the sink, which is cleaned up when DIAG_MGR
   is released.
   DST_STREAM is borrowed, and must outlive DIAG_MGR.
   The output for each diagnostic is written and flushed as each
   diagnostic is finished.  */

extern diagnostic_text_sink *
diagnostic_manager_add_text_sink (diagnostic_manager *diag_mgr,
				  FILE *dst_stream,
				  enum diagnostic_colorize colorize)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Functions to manipulate text sinks.  */

/* Enable/disable printing of source text in the text sink.
   Default: enabled.  */

extern void
diagnostic_text_sink_set_source_printing_enabled (diagnostic_text_sink *text_sink,
						  int value)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Update colorization of text sink.  */

extern void
diagnostic_text_sink_set_colorize (diagnostic_text_sink *text_sink,
				   enum diagnostic_colorize colorize)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Enable/disable colorization of the characters of source text
   that are underlined.
   This should be true for clients that generate range information
   (so that the ranges of code are colorized),
   and false for clients that merely specify points within the
   source code (to avoid e.g. colorizing just the first character in
   a token, which would look strange).
   Default: enabled.  */

extern void
diagnostic_text_sink_set_labelled_source_colorization_enabled (diagnostic_text_sink *text_sink,
							       int value)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Add a new output sink to DIAG_MGR, which writes SARIF of the given
   version to DST_STREAM.

   The output is not written until DIAG_MGR is released.

   DST_STREAM is borrowed, and must outlive DIAG_MGR.

   For the result to be a valid SARIF file according to the schema,
   DIAG_MGR must have had diagnostic_manager_set_tool_name called on it.  */

extern void
diagnostic_manager_add_sarif_sink (diagnostic_manager *diag_mgr,
				   FILE *dst_stream,
				   const diagnostic_file *main_input_file,
				   enum diagnostic_sarif_version version)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (3);

/* Write a patch to DST_STREAM consisting of all fix-it hints
   on all diagnostics that have been finished on DIAG_MGR.  */

extern void
diagnostic_manager_write_patch (diagnostic_manager *diag_mgr,
				FILE *dst_stream)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Location management.  */

/* Create a new diagnostic_file * for file NAME.

   Repeated calls with matching NAMEs will return the
   same object.

   If SARIF_SOURCE_LANGUAGE is non-NULL, it specifies a "sourceLanguage"
   value for the file when use when writing SARIF.
   See SARIF v2.1.0 Appendix J for suggested values for various
   programmming languages.  */

extern diagnostic_file *
diagnostic_manager_new_file (diagnostic_manager *diag_mgr,
			     const char *name,
			     const char *sarif_source_language)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (3);

/* Populate the source-quoting cache for FILE, specifying the
   given buffer as the content of the file (rather than
   attempting to read the content from the filesystem).  */

extern void
diagnostic_file_set_buffered_content (diagnostic_file *file,
				      const char *buf,
				      size_t sz)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Write a representation of FILE to OUT, for debugging.  */

extern void
diagnostic_manager_debug_dump_file (diagnostic_manager *diag_mgr,
				    const diagnostic_file *file,
				    FILE *out)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (3);

/* Attempt to create a diagnostic_location representing
   FILENAME:LINE_NUM, with no column information
   (thus "the whole line").  */

extern const diagnostic_physical_location *
diagnostic_manager_new_location_from_file_and_line (diagnostic_manager *diag_mgr,
						    const diagnostic_file *file,
						    diagnostic_line_num_t line_num)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Attempt to create a diagnostic_physical_location representing
   FILENAME:LINE_NUM:COLUMN_NUM.  */

extern const diagnostic_physical_location *
diagnostic_manager_new_location_from_file_line_column (diagnostic_manager *diag_mgr,
						       const diagnostic_file *file,
						       diagnostic_line_num_t line_num,
						       diagnostic_column_num_t column_num)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Attempt to create a diagnostic_physical_location representing a
   range within a source file, with a highlighted "caret" location.

   All must be within the same file, but they can be on different lines.

   For example, consider the location of the binary expression below:

     ...|__________1111111112222222
     ...|12345678901234567890123456
     ...|
     521|int sum (int foo, int bar)
     522|{
     523|   return foo + bar;
     ...|          ~~~~^~~~~
     524|}

   The location's caret is at the "+", line 523 column 15, but starts
   earlier, at the "f" of "foo" at column 11.  The finish is at the "r"
   of "bar" at column 19.  */

extern const diagnostic_physical_location *
diagnostic_manager_new_location_from_range (diagnostic_manager *diag_mgr,
					    const diagnostic_physical_location *loc_caret,
					    const diagnostic_physical_location *loc_start,
					    const diagnostic_physical_location *loc_end)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (3)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (4);

/* Write a representation of LOC to OUT, for debugging.  */

extern void
diagnostic_manager_debug_dump_location (const diagnostic_manager *diag_mgr,
					const diagnostic_physical_location *loc,
					FILE *out)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (3);

/* A bundle of state describing a logical location in the user's source,
   such as "in function 'foo'".

   SHORT_NAME can be NULL, or else a string suitable for use by
   the SARIF logicalLocation "name" property (SARIF v2.1.0 section 3.33.4).

   FULLY_QUALIFIED_NAME can be NULL or else a string  suitable for use by
   the SARIF logicalLocation "fullyQualifiedName" property
   (SARIF v2.1.0 section 3.33.5).

   DECORATED_NAME can be NULL or else a string  suitable for use by
   the SARIF logicalLocation "decoratedName" property
   (SARIF v2.1.0 section 3.33.6).  */

extern const diagnostic_logical_location *
diagnostic_manager_new_logical_location (diagnostic_manager *diag_mgr,
					 enum diagnostic_logical_location_kind_t kind,
					 const diagnostic_logical_location *parent,
					 const char *short_name,
					 const char *fully_qualified_name,
					 const char *decorated_name)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (3)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (4)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (5)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (6);

/* Write a representation of LOC to OUT, for debugging.  */

extern void
diagnostic_manager_debug_dump_logical_location (const diagnostic_manager *diag_mgr,
						const diagnostic_logical_location *loc,
						FILE *out)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (3);

/* Accessors for logical locations (added in LIBGDIAGNOSTICS_ABI_1;
   you can test for their presence using
   #ifdef LIBDIAGNOSTICS_HAVE_LOGICAL_LOCATION_ACCESSORS
*/
#define LIBDIAGNOSTICS_HAVE_LOGICAL_LOCATION_ACCESSORS

extern enum diagnostic_logical_location_kind_t
diagnostic_logical_location_get_kind (const diagnostic_logical_location *loc)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

extern const diagnostic_logical_location *
diagnostic_logical_location_get_parent (const diagnostic_logical_location *loc)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

extern const char *
diagnostic_logical_location_get_short_name (const diagnostic_logical_location *loc)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

extern const char *
diagnostic_logical_location_get_fully_qualified_name (const diagnostic_logical_location *loc)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

extern const char *
diagnostic_logical_location_get_decorated_name (const diagnostic_logical_location *loc)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Diagnostic groups.  */

/* Begin a diagnostic group.  All diagnostics emitted within
   DIAG_MGR after the first one will be treated as notes about
   the initial diagnostic.  */

extern void
diagnostic_manager_begin_group (diagnostic_manager *diag_mgr)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Finish a diagnostic group.  */

extern void
diagnostic_manager_end_group (diagnostic_manager *diag_mgr)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Step-by-step creation of a diagnostic.  */

extern diagnostic *
diagnostic_begin (diagnostic_manager *diag_mgr,
		  enum diagnostic_level level)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Associate this diagnostic with the given ID within
   the Common Weakness Enumeration.  */

extern void
diagnostic_set_cwe (diagnostic *diag,
		    unsigned cwe_id)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Associate this diagnostic with a particular rule that has been violated
   (such as in a coding standard, or within a specification).
   The rule must have at least one of a title and a URL, but these
   can be NULL.
   A diagnostic can be associated with zero or more rules.  */

extern void
diagnostic_add_rule (diagnostic *diag,
		     const char *title,
		     const char *url)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (3);

/* Set the primary location of DIAG.  */

extern void
diagnostic_set_location (diagnostic *diag,
			 const diagnostic_physical_location * loc)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2);

/* Set the primary location of DIAG, with a label.  */

extern void
diagnostic_set_location_with_label (diagnostic *diag,
				    const diagnostic_physical_location *loc,
				    const char *fmt, ...)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (3);

/* Add a secondary location to DIAG.  */

extern void
diagnostic_add_location (diagnostic *diag,
			 const diagnostic_physical_location * loc)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Add a secondary location to DIAG, with a label.  */

extern void
diagnostic_add_location_with_label (diagnostic *diag,
				    const diagnostic_physical_location *loc,
				    const char *text)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (3);

/* Set the logical location of DIAG.  */

extern void
diagnostic_set_logical_location (diagnostic *diag,
				 const diagnostic_logical_location *logical_loc)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2);

/* Fix-it hints.  */

extern void
diagnostic_add_fix_it_hint_insert_before (diagnostic *diag,
					  const diagnostic_physical_location *loc,
					  const char *addition)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (3);

extern void
diagnostic_add_fix_it_hint_insert_after (diagnostic *diag,
					 const diagnostic_physical_location *loc,
					 const char *addition)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (3);

extern void
diagnostic_add_fix_it_hint_replace (diagnostic *diag,
				    const diagnostic_physical_location *loc,
				    const char *replacement)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (3);

extern void
diagnostic_add_fix_it_hint_delete (diagnostic *diag,
				   const diagnostic_physical_location *loc)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2);

/* Create and borrow a pointer to an execution path for DIAG.
   The path is automatically cleaned up when DIAG is finished.  */

extern diagnostic_execution_path *
diagnostic_add_execution_path (diagnostic *diag)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Create a new execution path.
   This is owned by the called and must have either
   diagnostic_take_execution_path or diagnostic_execution_path_release
   called on it.  */

extern diagnostic_execution_path *
diagnostic_manager_new_execution_path (diagnostic_manager *manager)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Set DIAG to use PATH as its execution path, taking ownership of PATH.  */

extern void
diagnostic_take_execution_path (diagnostic *diag,
				diagnostic_execution_path *path)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Release ownership of PATH, which must not have been taken
   by a diagnostic.  */

extern void
diagnostic_execution_path_release (diagnostic_execution_path *path)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (1);

/* Append an event to the end of PATH.  */

extern diagnostic_event_id
diagnostic_execution_path_add_event (diagnostic_execution_path *path,
				     const diagnostic_physical_location *physical_loc,
				     const diagnostic_logical_location *logical_loc,
				     unsigned stack_depth,
				     const char *fmt, ...)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (3)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (5)
  LIBGDIAGNOSTICS_PARAM_GCC_FORMAT_STRING (5, 6);

/* Append an event to the end of PATH.  */

extern diagnostic_event_id
diagnostic_execution_path_add_event_va (diagnostic_execution_path *path,
					const diagnostic_physical_location *physical_loc,
					const diagnostic_logical_location *logical_loc,
					unsigned stack_depth,
					const char *fmt,
					va_list *args)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (3)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (5)
  LIBGDIAGNOSTICS_PARAM_GCC_FORMAT_STRING (5, 0);

/* Emit DIAG to all sinks of its manager, and release DIAG.
   Use FMT for the message.
   Note that this uses gcc's pretty-print format, which is *not* printf.
   TODO: who is responsible for putting FMT through gettext?  */

extern void
diagnostic_finish (diagnostic *diag, const char *fmt, ...)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2)
  LIBGDIAGNOSTICS_PARAM_GCC_FORMAT_STRING (2, 3);

/* As diagnostic_finish, but with a va_list.  */

extern void
diagnostic_finish_va (diagnostic *diag, const char *fmt, va_list *args)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2)
  LIBGDIAGNOSTICS_PARAM_GCC_FORMAT_STRING (2, 0);

/* Get the diagnostic_file associated with PHYSICAL_LOC.  */

extern diagnostic_file *
diagnostic_physical_location_get_file (const diagnostic_physical_location *physical_loc)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL(0);

/* Attempt to parse SPEC as if an argument to GCC's
   -fdiagnostics-add-output=OUTPUT-SPEC.
   If successful, add an output sink to AFFECTED_MGR and return zero.
   Otherwise, emit a diagnostic to CONTROL_MGR and return non-zero.
   Added in LIBGDIAGNOSTICS_ABI_2.  */
#define LIBDIAGNOSTICS_HAVE_diagnostic_manager_add_sink_from_spec

extern int
diagnostic_manager_add_sink_from_spec (diagnostic_manager *affected_mgr,
				       const char *option_name,
				       const char *spec,
				       diagnostic_manager *control_mgr)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (3)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (4);


/* Set the main input file of MGR to be FILE.
   This affects the <title> of generated HTML and
   the "role" of the artifact in SARIF output (SARIF v2.1.0
   section 3.24.6).
   Added in LIBGDIAGNOSTICS_ABI_2.  */
#define LIBDIAGNOSTICS_HAVE_diagnostic_manager_set_analysis_target

extern void
diagnostic_manager_set_analysis_target (diagnostic_manager *mgr,
					const diagnostic_file *file)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Directed graphs.  */

typedef struct diagnostic_graph diagnostic_graph;
typedef struct diagnostic_node diagnostic_node;
typedef struct diagnostic_edge diagnostic_edge;

/* Create a new graph.
   This is owned by the caller and must have one of
   diagnostic_manager_take_global_graph, diagnostic_take_graph,
   or diagnostic_graph_release called on it.
   Added in LIBGDIAGNOSTICS_ABI_3.  */

extern diagnostic_graph *
diagnostic_manager_new_graph (diagnostic_manager *manager)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Report this graph "globally", taking ownership of it.
   Added in LIBGDIAGNOSTICS_ABI_3.  */

extern void
diagnostic_manager_take_global_graph (diagnostic_manager *manager,
				      diagnostic_graph *graph)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Add this graph to DIAG, transferring ownership to it.
   Added in LIBGDIAGNOSTICS_ABI_3.  */

extern void
diagnostic_take_graph (diagnostic *diag,
		      diagnostic_graph *graph)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Release this graph.  Added in LIBGDIAGNOSTICS_ABI_3.  */

extern void
diagnostic_graph_release (diagnostic_graph *graph)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (1);

/* Set the description of GRAPH for use
   in the value of the SARIF "description" property
   (SARIF v2.1.0 section 3.39.2).
   Added in LIBGDIAGNOSTICS_ABI_3.  */

extern void
diagnostic_graph_set_description (diagnostic_graph *graph,
				 const char *description)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2);

/* Create and add a new node within GRAPH.
   NODE_ID must be unique within nodes in GRAPH.
   The new node is owned by GRAPH.
   PARENT_NODE can be NULL (for a top-level node in the graph),
   or non-null for a child node.
   Added in LIBGDIAGNOSTICS_ABI_3.  */

extern diagnostic_node *
diagnostic_graph_add_node (diagnostic_graph *graph,
			   const char *node_id,
			   diagnostic_node *parent_node)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (3);

/* Create and add a new edge within GRAPH.

   If non-null, then EDGE_ID must be unique within edges in GRAPH;
   if EDGE_ID is null then a unique id of the form "edge0", "edge1", etc
   will be used automatically.

   The new edge is owned by GRAPH.
   Added in LIBGDIAGNOSTICS_ABI_3.  */

extern diagnostic_edge *
diagnostic_graph_add_edge (diagnostic_graph *graph,
			   const char *edge_id,
			   diagnostic_node *src_node,
			   diagnostic_node *dst_node,
			   const char *label)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (3)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (4)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (5);

/* Get the node in GRAPH with the given id, or null.
   Added in LIBGDIAGNOSTICS_ABI_3.  */

extern diagnostic_node *
diagnostic_graph_get_node_by_id (diagnostic_graph *graph,
				 const char *node_id)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Get the edge in GRAPH with the given id, or null.
   Added in LIBGDIAGNOSTICS_ABI_3.  */

extern diagnostic_edge *
diagnostic_graph_get_edge_by_id (diagnostic_graph *graph,
				 const char *edge_id)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Set the label of NODE for use
   in the value of the SARIF "label" property
   (SARIF v2.1.0 section 3.40.3).
   Added in LIBGDIAGNOSTICS_ABI_3.  */

extern void
diagnostic_node_set_label (diagnostic_node *node,
			   const char *label)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2);

/* Set the physical location of NODE.
   Added in LIBGDIAGNOSTICS_ABI_3.  */

extern void
diagnostic_node_set_location (diagnostic_node *node,
			      const diagnostic_physical_location *loc)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2);

/* Set the logical location of NODE.
   Added in LIBGDIAGNOSTICS_ABI_3.  */

extern void
diagnostic_node_set_logical_location (diagnostic_node *node,
				      const diagnostic_logical_location *logical_loc)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2);

/* Message buffers.  */

#define LIBDIAGNOSTICS_HAVE_diagnostic_message_buffer

/* Create a new diagnostic_message_buffer.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern diagnostic_message_buffer *
diagnostic_message_buffer_new (void);

/* Release a diagnostic_message_buffer that hasn't been used.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_message_buffer_release (diagnostic_message_buffer *msg_buf)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Append a UTF-8 encoded null-terminated string to the buffer.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_message_buffer_append_str (diagnostic_message_buffer *msg_buf,
				      const char *p)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Append a UTF-8 encoded run of bytes to the buffer.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_message_buffer_append_text (diagnostic_message_buffer *msg_buf,
				       const char *p,
				       size_t len)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* Append a byte to to the buffer.  This should be either
   ASCII, or part of UTF-8 encoded text.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_message_buffer_append_byte (diagnostic_message_buffer *msg_buf,
				       char ch)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Append a formatted string to the buffer, using the formatting rules
   for "printf".
   The string is assumed to be UTF-8 encoded.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_message_buffer_append_printf (diagnostic_message_buffer *msg_buf,
					 const char *fmt, ...)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_PRINTF_FORMAT_STRING (2, 3);

/* Append a diagnostic_event_id to the buffer in the form "(1)".
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_message_buffer_append_event_id (diagnostic_message_buffer *msg_buf,
					   diagnostic_event_id event_id)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Begin a run of text associated with the given URL.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_message_buffer_begin_url (diagnostic_message_buffer *msg_buf,
				     const char *url)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* End a run of text started with diagnostic_message_buffer_begin_url.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_message_buffer_end_url (diagnostic_message_buffer *msg_buf)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Begin a run of text to be printed in quotes.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_message_buffer_begin_quote (diagnostic_message_buffer *msg_buf)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* End a run of text started with diagnostic_message_buffer_begin_quote.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_message_buffer_end_quote (diagnostic_message_buffer *msg_buf)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Begin a run of text to be printed with color.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_message_buffer_begin_color (diagnostic_message_buffer *msg_buf,
				       const char *color)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* End a run of text started with diagnostic_message_buffer_begin_color.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_message_buffer_end_color (diagnostic_message_buffer *msg_buf)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1);

/* Write a debugging representation of MSG_BUG to OUTF.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_message_buffer_dump (const diagnostic_message_buffer *msg_buf,
				FILE *outf)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* As diagnostic_finish, but takes ownership of MSG_BUF.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_finish_via_msg_buf (diagnostic *diag,
			       diagnostic_message_buffer *msg_buf)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (2);

/* As diagnostic_add_location_with_label but takes ownership of MSG_BUF.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_add_location_with_label_via_msg_buf (diagnostic *diag,
						const diagnostic_physical_location *loc,
						diagnostic_message_buffer *msg_buf)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (3);

/* As diagnostic_execution_path_add_event but takes ownership of MSG_BUF.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern diagnostic_event_id
diagnostic_execution_path_add_event_via_msg_buf (diagnostic_execution_path *path,
						 const diagnostic_physical_location *physical_loc,
						 const diagnostic_logical_location *logical_loc,
						 unsigned stack_depth,
						 diagnostic_message_buffer *msg_buf)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (3)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (5);

/* Set the description of GRAPH for use
   in the value of the SARIF "description" property
   (SARIF v2.1.0 section 3.39.2).

   Takes ownership of DESC, if non-null.

   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_graph_set_description_via_msg_buf (diagnostic_graph *graph,
					      diagnostic_message_buffer *desc)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2);

/* Create and add a new edge within GRAPH.

   If non-null, then EDGE_ID must be unique within edges in GRAPH;
   if EDGE_ID is null then a unique id of the form "edge0", "edge1", etc
   will be used automatically.

   Takes ownership of LABEL, if non-null.

   The new edge is owned by GRAPH.
   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern diagnostic_edge *
diagnostic_graph_add_edge_via_msg_buf (diagnostic_graph *graph,
				       const char *edge_id,
				       diagnostic_node *src_node,
				       diagnostic_node *dst_node,
				       diagnostic_message_buffer *label)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (3)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (4)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (5);

/* Set the label of NODE for use
   in the value of the SARIF "label" property
   (SARIF v2.1.0 section 3.40.3).

   Takes ownership of LABEL, if non-null.

   Added in LIBGDIAGNOSTICS_ABI_4.  */

extern void
diagnostic_node_set_label_via_msg_buf (diagnostic_node *node,
				       diagnostic_message_buffer *label)
  LIBGDIAGNOSTICS_PARAM_MUST_BE_NON_NULL (1)
  LIBGDIAGNOSTICS_PARAM_CAN_BE_NULL (2);

/* If non-zero, print debugging information to stderr when
   creating diagnostic_physical_location instances.

   Added in LIBGDIAGNOSTICS_ABI_5.  */
#define LIBDIAGNOSTICS_HAVE_diagnostic_manager_set_debug_physical_locations

extern void
diagnostic_manager_set_debug_physical_locations (diagnostic_manager *mgr,
						 int value);

/* DEFERRED:
   - thread-safety
   - plural forms
   - enum about what a "column number" means (bytes, unichars, etc)
   - locations within binary files
   - options and URLs for warnings
   - enable/disable of warnings by kind
   - command-line arguments
   - plugin metadata.  */

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif  /* LIBGDIAGNOSTICS_H  */
