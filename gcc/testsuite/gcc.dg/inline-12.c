/* Test misuses of inline.  -pedantic-errors test.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic-errors" } */

/* These should perhaps be hard errors, but are pedwarns at
   present.  */

inline int a; /* { dg-error "variable 'a' declared 'inline'" } */
inline int (*b)(void); /* { dg-error "variable 'b' declared 'inline'" } */
typedef inline void c(void); /* { dg-error "typedef 'c' declared 'inline'" } */
typedef inline int d; /* { dg-error "typedef 'd' declared 'inline'" } */
void e(inline int f(void)); /* { dg-error "parameter 'f' declared 'inline'" } */
void g(inline int(void)); /* { dg-error "parameter '\\({anonymous}\\)' declared 'inline'" } */
