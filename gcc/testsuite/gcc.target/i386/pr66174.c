/* { dg-do compile } */
/* { dg-options "-O -ftree-vectorize -mavx512f" } */

extern void abort (void);

typedef struct {
   unsigned int a;
   unsigned int b;
} ii;

void foo (unsigned short *arr, ii *iarr)
{
  ii *iptr = iarr;
  unsigned short res[128];
  ii ires[128];
  int i;
  for (i = 0; i < 128; i++)
    {
      ires[i].a = iptr->b - iptr->a;
      ires[i].b = iptr->b + iptr->a;
      iptr++;
    }
  for (i = 0; i < 128; i++)
    {
      if (res[i] != arr[i]
          || ires[i].a != iarr[i].b - iarr[i].a
          || ires[i].b != iarr[i].b + iarr[i].a)
        abort ();
    }
}
