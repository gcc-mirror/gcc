/* { dg-do compile } */
/* { dg-options "-O2 -favoid-store-forwarding -fdump-rtl-avoid_store_forwarding" } */

typedef union {
    char arr_8[8];
    int long_value;
} DataUnion1;

long no_ssll_1 (DataUnion1 *data, char x)
{
  data->arr_8[4] = x;
  return data->long_value;
}

long no_ssll_2 (DataUnion1 *data, char x)
{
  data->arr_8[5] = x;
  return data->long_value;
}

typedef union {
    char arr_8[8];
    short long_value[4];
} DataUnion2;

long no_ssll_3 (DataUnion2 *data, char x)
{
  data->arr_8[4] = x;
  return data->long_value[1];
}

long no_ssll_4 (DataUnion2 *data, char x)
{
  data->arr_8[0] = x;
  return data->long_value[1];
}

/* { dg-final { scan-rtl-dump-times "Store forwarding detected" 0 "avoid_store_forwarding" } } */
/* { dg-final { scan-rtl-dump-times "Store forwarding avoided" 0 "avoid_store_forwarding" } } */
