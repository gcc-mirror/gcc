/* Test misuses of inline.  -pedantic-errors test.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic-errors" } */

/* These should perhaps be hard errors, but are pedwarns at
   present.  */

inline int a; /* { dg-error "error: variable 'a' declared 'inline'" } */
inline int (*b)(void); /* { dg-error "error: variable 'b' declared 'inline'" } */
typedef inline void c(void); /* { dg-error "error: typedef 'c' declared 'inline'" } */
typedef inline int d; /* { dg-error "error: typedef 'd' declared 'inline'" } */
void e(inline int f(void)); /* { dg-error "error: parameter 'f' declared 'inline'" } */
void g(inline int(void)); /* { dg-error "error: parameter '\\({anonymous}\\)' declared 'inline'" } */
