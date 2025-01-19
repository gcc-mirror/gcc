/* { dg-do compile } */
/* { dg-options "-O2 -favoid-store-forwarding -fdump-rtl-avoid_store_forwarding" } */

typedef int v4si __attribute__ ((vector_size (16)));

typedef union {
    char arr_16[16];
    v4si vec_value;
} DataUnion;

v4si ssll_vect_1 (DataUnion *data, char x)
{
  data->arr_16[0] = x;
  return data->vec_value;
}

v4si ssll_vect_2 (DataUnion *data, int x)
{
  __builtin_memcpy(data->arr_16 + 4, &x, sizeof(int));
  return data->vec_value;
}

/* Scalar stores leading to vector loads cause store_bit_field to generate
   subreg expressions on different register classes. This should be handled
   using vec_duplicate, so it is marked as an XFAIL for now.  */
/* { dg-final { scan-rtl-dump-times "Store forwarding detected" 2 "avoid_store_forwarding" { xfail *-*-* } } } */
