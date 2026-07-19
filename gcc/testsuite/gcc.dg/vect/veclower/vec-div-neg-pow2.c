/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect128 } */

/* Division by a negative power of two is a shift and a negation, and the
   remainder a mask, as for a positive divisor; no multiply-high.  */

typedef int v4si __attribute__ ((vector_size (16)));
typedef short v8hi __attribute__ ((vector_size (16)));

v4si
f1 (v4si x)
{
  return x / -32;
}

v8hi
f2 (v8hi x)
{
  return x / -8;
}

v4si
f3 (v4si x)
{
  return x % -32;
}

v4si
f4 (v4si x)
{
  return x % (v4si) { -4, 4, -4, 4 };
}

/* { dg-final { scan-tree-dump-not " h\\* " "veclower21" } } */
/* Targets with vector division keep it.  */
/* { dg-final { scan-tree-dump-times " = -_| / \{ -" 2 "veclower21" } } */
