/* { dg-do compile }
 * { dg-options "-std=gnu23 -O1" } */

extern void abort(void);

int test(int *n, struct T { char a[*n], b[*n]; }*) {
  return sizeof(struct T) - sizeof(struct T);
}

void f1(int *p) {
  if (test(p, 0)) abort();
}

