/* PR c/122982 */ 
/* { dg-do compile } */
/* { dg-options "-O0" } */

int* f (int);

struct __bounded_ptr {
 int k;
 int *buf __attribute__ ((counted_by (k)));
};

int*
f1 (int n) { return f (n); }

void h1 (void)
{ 
  int *p = (struct __bounded_ptr) {3, f1 (3)}.buf;
  __builtin_memset (p, 0, 3 * sizeof p);
}
