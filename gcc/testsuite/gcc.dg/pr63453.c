/* PR c/63453 */
/* { dg-do compile } */
/* { dg-options "-std=gnu11" } */

inline int fn1 (void); /* { dg-warning "declared but never defined" } */
extern inline int fn2 (void); /* { dg-warning "declared but never defined" } */
inline int __attribute__ ((gnu_inline)) fn3 (void);
extern inline int __attribute__ ((gnu_inline)) fn4 (void);
