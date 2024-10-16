/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -favoid-store-forwarding -fdump-rtl-avoid_store_forwarding" } */

typedef float v4f __attribute__ ((vector_size (16)));

typedef union {
    float arr_2[4];
    long long_value;
    __int128 longlong_value;
    v4f vec_value;
} DataUnion;

long ssll_load_elim_1 (DataUnion *data, float x)
{
  data->arr_2[0] = x;
  data->arr_2[1] = 0.0f;
  return data->long_value;
}

__int128 ssll_load_elim_2 (DataUnion *data, float x)
{
  data->arr_2[0] = x;
  data->arr_2[1] = 0.0f;
  data->arr_2[2] = x;
  data->arr_2[3] = 0.0f;
  return data->longlong_value;
}

v4f ssll_load_elim_3 (DataUnion *data, float x)
{
  data->arr_2[3] = x;
  data->arr_2[2] = x;
  data->arr_2[1] = x;
  data->arr_2[0] = x;
  return data->vec_value;
}

/* Scalar stores leading to vector loads cause store_bit_field to generate
   subreg expressions on different register classes. This should be handled
   using vec_duplicate, so it is marked as an XFAIL for now.  */
/* { dg-final { scan-rtl-dump-times "Store forwarding detected" 3 "avoid_store_forwarding" { xfail *-*-* } } } */
