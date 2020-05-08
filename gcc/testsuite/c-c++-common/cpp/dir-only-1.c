/* Copyright 2007 Free Software Foundation, Inc.
   Contributed by Ollie Wild <aaw@google.com>.  */

/* { dg-do preprocess } */
/* { dg-options -fdirectives-only } */

/* Tests scan_translation_unit_directives_only()'s handling of corner cases. */

/* Ignore directives inside block comments...
#error directive inside block comment
*/

// Escaped newline doesn't terminate line comment \
#error directive inside line comment

/* A comment canot start inside a string. */
const char *c1 = "/*";
#define NOT_IN_COMMENT
const char *c2 = "*/";
#ifndef NOT_IN_COMMENT
#error Comment started inside a string literal
#endif

/* Escaped newline handling. */
int i; \
#error ignored escaped newline
  \
  \
#define BOL
#ifndef BOL
#error escaped newline did not preserve beginning of line
#endif

/* Handles \\ properly at the end of a string. */
"string ends in \\"/*
#error Missed string terminator.
*/

/* Handles macro expansion in preprocessing directives. */
#define HEADER "dir-only-1.h"
#include HEADER
#ifndef GOT_HEADER
#error Failed to include header.
#endif

/\
*
#define IN_COMMENT
*/
#ifdef IN_COMMENT
#error Escaped newline breaks block comment initiator.
#endif

/*
*\
/
#define NOT_IN_COMMENT2
/**/
#ifndef NOT_IN_COMMENT2
#error Escaped newline breaks block comment terminator.
#endif

/* Test escaped newline inside character escape sequence. */
"\\
\"/*
#error Missed string terminator
*/

/* Block comments don't mask trailing preprocessing
   directive. */ #define NOT_MASKED
#ifndef NOT_MASKED
#error Comment masks trailing directive.
#endif
