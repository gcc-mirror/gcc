/* { dg-do compile } */
/* { dg-options "-O2 -std=c99" } */
#if(__SIZEOF_INT__ >= 4)
char heap[50000];
#else
char heap[32000];
#endif
int
main ()
{
  for (unsigned ix = sizeof (heap); ix--;)
    heap[ix] = ix;
  return 0;
}
