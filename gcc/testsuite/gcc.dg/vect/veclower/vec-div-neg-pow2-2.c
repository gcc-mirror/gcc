/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_condition } */
/* { dg-require-effective-target vect128 } */

/* A divisor element of -1 is -2^0: the remainder is a mask, as for the
   other power-of-two elements, not an element-wise expansion.  */

typedef int v4si __attribute__ ((vector_size (16)));
typedef short v8hi __attribute__ ((vector_size (16)));

v4si
f1 (v4si x)
{
  return x % (v4si) { -4, 2, -1, 8 };
}

v8hi
f2 (v8hi x)
{
  return x % (v8hi) { -1, 2, -4, 8, -16, 32, -64, 128 };
}

/* { dg-final { scan-tree-dump-not "BIT_FIELD_REF" "veclower21" } } */
/* { dg-final { scan-tree-dump-not " h\\* " "veclower21" } } */
