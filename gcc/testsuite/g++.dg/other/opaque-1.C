/* { dg-do run { target powerpc-*-eabispe* } } */

#define __vector __attribute__((vector_size(8)))
typedef float __vector __ev64_fs__;

__ev64_fs__ f;
__ev64_opaque__ o;

int here = 0;

void bar (__ev64_opaque__ x)
{
  here = 0;
}

void bar (__ev64_fs__ x)
{ 
  here = 888;
}

int main ()
{
  f = o;
  o = f;
  bar (f);
  if (here != 888)
    return 1;
  return 0;
}
