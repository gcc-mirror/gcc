/* PR preprocessor/29612 */
/* { dg-do compile } */
/* { dg-options "" } */

# 6 "pr29612-1.c"

int foo (void) { return 'ab'; } /* { dg-warning "multi-character" } */

# 1 "foo.h" 1 3

int bar (void) { return 'ab'; } /* No warning in system header.  */

# 14 "pr29612-1.c" 2

int baz (void) { return 'ab'; } /* { dg-warning "multi-character" } */
