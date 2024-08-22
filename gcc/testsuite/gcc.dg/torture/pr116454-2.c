/* { dg-do run } */
/* { dg-additional-options "-ffast-math" } */
_Complex float arr[2];

int main() {
  _Complex float *ptr;
  ptr = arr;
  *++ptr * 1j; 
  /* ptr should only increment once, not twice. */
  if (ptr != arr + 1)
    __builtin_abort ();
}
