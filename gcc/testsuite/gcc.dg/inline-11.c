/* Test misuses of inline.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* These should perhaps be hard errors, but are pedwarns at
   present.  */

inline int a; /* { dg-warning "warning: variable 'a' declared 'inline'" } */
inline int (*b)(void); /* { dg-warning "warning: variable 'b' declared 'inline'" } */
typedef inline void c(void); /* { dg-warning "warning: typedef 'c' declared 'inline'" } */
typedef inline int d; /* { dg-warning "warning: typedef 'd' declared 'inline'" } */
void e(inline int f(void)); /* { dg-warning "warning: parameter 'f' declared 'inline'" } */
void g(inline int(void)); /* { dg-warning "warning: parameter '\\({anonymous}\\)' declared 'inline'" } */
