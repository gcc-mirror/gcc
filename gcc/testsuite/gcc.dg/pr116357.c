/* PR c/116357 */
/* { dg-do compile } */
/* { dg-options "" } */

typedef int A __attribute__((aligned (2 * sizeof (int))));
A a[4];	/* { dg-error "alignment of array elements is greater than element size" } */
typedef volatile int B __attribute__((aligned (2 * sizeof (int))));
B b[4];	/* { dg-error "alignment of array elements is greater than element size" } */
typedef const int C __attribute__((aligned (2 * sizeof (int))));
C c[4];	/* { dg-error "alignment of array elements is greater than element size" } */
