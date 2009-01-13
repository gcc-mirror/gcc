/* { dg-do run { target { powerpc*-*-* && vmx_hw } } } */
/* { dg-do compile { target { powerpc*-*-* && { ! vmx_hw } } } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */
/* Test the vec_extract VMX intrinsics.  */
#include <altivec.h>

extern void abort (void);

vector int a = {0, 1, 2, 3};
vector short b = {0, 1, 2, 3, 4, 5, 6, 7};

int f(vector int a, int b)
{
  return vec_extract (a, b);
}

int f0 (vector int a)
{
  return vec_extract (a, 0);
}
int f1 (vector int a)
{
  return vec_extract (a, 1);
}
int f2 (vector int a)
{
  return vec_extract (a, 2);
}
int f3 (vector int a)
{
  return vec_extract (a, 3);
}
int f4 (vector int a)
{
  return vec_extract (a, 4);
}

int g(vector short a, int b)
{
  return vec_extract (a, b);
}

int g0 (vector short a)
{
  return vec_extract (a, 0);
}
int g1 (vector short a)
{
  return vec_extract (a, 1);
}
int g2 (vector short a)
{
  return vec_extract (a, 2);
}
int g3 (vector short a)
{
  return vec_extract (a, 3);
}

int g4 (vector short a)
{ 
  return vec_extract (a, 4);
}
int g5 (vector short a)
{ 
  return vec_extract (a, 5);
}
int g6 (vector short a)
{ 
  return vec_extract (a, 6);
}
int g7 (vector short a)
{ 
  return vec_extract (a, 7);
}
int g8 (vector short a)
{ 
  return vec_extract (a, 8);
}
int main1(void) __attribute__((noinline));
int main1(void)
{ 
  int i;
  /* Check vec_extract with a non constant element numbering */
  for(i=0;i<10;i++)
    { 
      if (f(a, i) != (i&0x3))
        abort ();
    }
  
  /* Check vec_extract with a constant element numbering */
  if (f0(a) != 0)
    abort ();
  if (f1(a) != 1)
    abort ();
  if (f2(a) != 2)
    abort ();
  if (f3(a) != 3)
    abort ();
  /* Check that vec_extract works with a constant element higher than
     the number of elements.  */
  if (f4(a) != 0)
    abort ();

  /* Check vec_extract with a non constant element numbering */
  for(i=0;i<10;i++)
    {
      if (g(b, i) != (i&0x7))
        abort ();
    }
  
  /* Check vec_extract with a constant element numbering */
  if (g0(b) != 0)
    abort ();
  if (g1(b) != 1)
    abort ();
  if (g2(b) != 2)
    abort ();
  if (g3(b) != 3)
    abort ();
  if (g4(b) != 4)
    abort ();
  if (g5(b) != 5)
    abort ();
  if (g6(b) != 6)
    abort ();
  if (g7(b) != 7)
    abort ();
  /* Check that vec_extract works with a constant element higher than
     the number of elements.  */
  if (g8(b) != 0)
    abort ();
  
  return 0;
}

int main(void)
{ 
  return main1 ();
}
