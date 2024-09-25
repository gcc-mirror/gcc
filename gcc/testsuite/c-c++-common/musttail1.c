/* { dg-do compile { target { musttail && { c || c++11 } } } } */
/* { dg-additional-options "-fdelayed-branch" { target sparc*-*-* } } */

int __attribute__((noinline,noclone,noipa))
callee (int i)
{
  return i * i;
}

int __attribute__((noinline,noclone,noipa))
caller (int i)
{
  [[gnu::musttail]] return callee (i + 1);
}
