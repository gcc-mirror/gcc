/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */

struct asCArray {
  unsigned *array;
  int length;
};
unsigned asCReaderTranslateFunction(struct asCArray b, unsigned t)
{
  int size = 0;
  for (unsigned num; num < t; num++)
  {
    if (num >= b.length)
      __builtin_abort();
    size += b.array[num];
  }
  return size;
}
