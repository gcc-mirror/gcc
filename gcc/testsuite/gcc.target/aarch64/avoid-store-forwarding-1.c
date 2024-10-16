/* { dg-do compile } */
/* { dg-options "-O2 -favoid-store-forwarding" } */

typedef union {
    char arr_8[8];
    long long_value;
} DataUnion;

long ssll_1 (DataUnion *data, char x)
{
  data->arr_8[0] = x;
  return data->long_value;
}

long ssll_2 (DataUnion *data, char x)
{
  data->arr_8[1] = x;
  return data->long_value;
}

long ssll_3 (DataUnion *data, char x)
{
  data->arr_8[7] = x;
  return data->long_value;
}

/* { dg-final { scan-assembler-times {ldr\tx[0-9]+, \[x[0-9]+\]\n\tstrb\tw[0-9]+, \[x[0-9]+(, \d+)?\]\n\tbfi\tx[0-9]+, x[0-9]+, \d+, \d+} 3 } } */
