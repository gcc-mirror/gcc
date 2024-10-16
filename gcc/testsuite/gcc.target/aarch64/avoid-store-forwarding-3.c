/* { dg-do compile } */
/* { dg-options "-O2 -favoid-store-forwarding" } */

typedef union {
    char arr_8[8];
    long long_value;
} DataUnion;

long ssll_multi_1 (DataUnion *data, char x)
{
  data->arr_8[0] = x;
  data->arr_8[2] = x;
  return data->long_value;
}

long ssll_multi_2 (DataUnion *data, char x)
{
  data->arr_8[0] = x;
  data->arr_8[1] = 11;
  return data->long_value;
}

long ssll_multi_3 (DataUnion *data, char x, short y)
{
  data->arr_8[1] = x;
  __builtin_memcpy(data->arr_8 + 4, &y, sizeof(short));
  return data->long_value;
}

/* { dg-final { scan-assembler-times {(\tstr[bh]\tw[0-9]+, \[x[0-9]+(, \d+)?\]\n){2}(\tbfi\tx[0-9]+, x[0-9]+, \d+, \d+\n){2}} 3 } } */
