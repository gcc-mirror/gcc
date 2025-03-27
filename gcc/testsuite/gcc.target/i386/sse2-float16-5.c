/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mno-sse2" } */

_Float16 a;
__bf16 c;
_Complex ac;
void
foo (_Float16 p)
{
  a = p;
}

void
foo1 (__bf16 p)
{
  c = p;
}


void
foo2 (_Complex p)
{
  ac = p;
}
