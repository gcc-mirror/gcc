/* { dg-do compile { target musttail } } */
/* { dg-additional-options "-fdelayed-branch" { target sparc*-*-* } } */

int __attribute__((noinline,noclone,noipa))
callee (int i)
{
  return i * i;
}

int __attribute__((noinline,noclone,noipa))
caller (int i)
{
  __attribute__((musttail)) return callee (i + 1);
}
