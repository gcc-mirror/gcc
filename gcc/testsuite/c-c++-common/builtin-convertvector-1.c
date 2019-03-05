typedef int v8si __attribute__((vector_size (8 * sizeof (int))));
typedef long long v4di __attribute__((vector_size (4 * sizeof (long long))));

void
foo (v8si *x, v4di *y, int z)
{
  __builtin_convertvector (*y, v8si);	/* { dg-error "number of elements of the first argument vector and the second argument vector type should be the same" } */
  __builtin_convertvector (*x, v4di);	/* { dg-error "number of elements of the first argument vector and the second argument vector type should be the same" } */
  __builtin_convertvector (*x, int);	/* { dg-error "second argument must be an integer or floating vector type" } */
  __builtin_convertvector (z, v4di);	/* { dg-error "first argument must be an integer or floating vector" } */
  __builtin_convertvector ();		/* { dg-error "expected" } */
  __builtin_convertvector (*x);		/* { dg-error "expected" } */
  __builtin_convertvector (*x, *y);	/* { dg-error "expected" } */
  __builtin_convertvector (*x, v8si, 1);/* { dg-error "expected" } */
}
