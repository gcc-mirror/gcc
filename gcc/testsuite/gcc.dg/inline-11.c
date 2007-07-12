/* Test misuses of inline.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* These should perhaps be hard errors, but are pedwarns at
   present.  */

inline int a; /* { dg-warning "variable 'a' declared 'inline'" } */
inline int (*b)(void); /* { dg-warning "variable 'b' declared 'inline'" } */
typedef inline void c(void); /* { dg-warning "typedef 'c' declared 'inline'" } */
typedef inline int d; /* { dg-warning "typedef 'd' declared 'inline'" } */
void e(inline int f(void)); /* { dg-warning "parameter 'f' declared 'inline'" } */
void g(inline int(void)); /* { dg-warning "parameter '\\({anonymous}\\)' declared 'inline'" } */
