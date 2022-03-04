/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

/* Basic test for the new VMX intrinsics.  */
#include <altivec.h>

int f(vector int a, int b)
{
  return vec_extract (a, b);
}
short f1(vector short a, int b)
{
  return vec_extract (a, b);
}
vector short f2(vector short a, int b)
{
  return vec_insert (b, a, b);
}
vector float f3(vector float a, int b)
{
  return vec_insert (b, a, b);
}

float g(void);

vector float f4(float b, int t)
{
  return vec_promote (g(), t);
}
vector float f5(float b)
{
  return vec_splats (g());
}




template <int> 
int tf(vector int a, int b)
{
  return vec_extract (a, b);
}
template <int> 
short tf1(vector short a, int b)
{
  return vec_extract (a, b);
}
template <int> 
vector short tf2(vector short a, int b)
{
  return vec_insert (b, a, b);
}
template <int> 
vector float tf3(vector float a, int b)
{
  return vec_insert (b, a, b);
}

template <int> 
vector float tf4(float b, int t)
{
  return vec_promote (g(), t);
}
template <int> 
vector float tf5(float b)
{
  return vec_splats (g());
}

int t(vector int a, int b)
{
  return tf<1>(a, b);
}
short t1(vector short a, int b)
{
  return tf1<1>(a, b);
}
vector short t2(vector short a, int b)
{
  return tf2<1>(a, b);
}
vector float t3(vector float a, int b)
{
  return tf3<1>(a, b);
}
vector float t4(float b, int t)
{
  return tf4<1>(b, t);
}
vector float t5(float b)
{
  return tf5<1>(b);
}
