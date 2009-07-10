// { dg-do compile }
// { dg-options "-O2 -fprefetch-loop-arrays -msse2" { target i?86-*-* x86_64-*-* } }

struct DOMStringHandle
{
  unsigned int fLength;
  int fRefCount;
};
static void *freeListPtr;
void foo(DOMStringHandle *dsg)
{
  int i;
  for (i = 1; i < 1023; i++)
    {
      *(void **) &dsg[i] = freeListPtr;
      freeListPtr = &dsg[i];
    }
}
