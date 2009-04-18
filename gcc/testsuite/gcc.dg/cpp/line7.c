/* Test #line syntax, including macro expansion and empty macros.  PR
   39647.  */

/* { dg-do preprocess } */
/* { dg-options "-std=c99 -pedantic-errors" } */

#define EMPTY_OBJ
#define EMPTY_FUNC()

#define A 12 "line7.c"
#line A

#line EMPTY_OBJ /* { dg-error "unexpected end of file" } */
#line X /* { dg-error "is not a positive integer" } */
#line 16 16 /* { dg-error "is not a valid filename" } */

#line 18 "line7.c" EMPTY_OBJ
#line 19 "line7.c" EMPTY_FUNC()
#line 20 "line7.c" foo /* { dg-error "extra tokens at end" } */
