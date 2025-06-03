/* { dg-do run } */
/* { dg-require-effective-target aarch64_big_endian } */
/* { dg-options "-O2 -favoid-store-forwarding" } */

typedef union {
    char arr[2];
    short value;
} DataUnion;

short __attribute__ ((noinline))
ssll (DataUnion *data, char x, char y)
{
  data->arr[0] = x;
  data->arr[1] = y;
  return data->value;
}

int main () {
  DataUnion data = {};
  short value = ssll (&data, 0, 1);
  if (value != 1)
    __builtin_abort ();
}