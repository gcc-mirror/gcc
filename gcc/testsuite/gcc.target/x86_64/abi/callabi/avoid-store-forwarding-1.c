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

/* Check that the order of stores and loads has changed.  */
/* { dg-final { scan-assembler-times {movq\t\(%[a-z]{3}\), %[a-z]{3}\n(\tmovl\t%[a-z]{3}, %[a-z]{3}\n)?\tmovb\t%[a-z]{3}, (\d+)?\(%[a-z]{3}\)} 2 } } */
