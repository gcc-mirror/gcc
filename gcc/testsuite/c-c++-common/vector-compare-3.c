/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef int v4i __attribute__((vector_size(4*sizeof(int))));

// fold should not turn (vec_other)(x<y) into (x<y)?vec_other(-1):vec_other(0).

void use (v4i const *z);

void
f (v4i *x, v4i *y)
{
  v4i const zz = *x < *y;
  use (&zz);
}

// Optimizations shouldn't introduce a boolean type in there

void
g (v4i *x, v4i const *y, v4i *z, v4i *t)
{
  *z = *x < *y | *x == *y;
  *t = *x < *y & *x > *y;
}

