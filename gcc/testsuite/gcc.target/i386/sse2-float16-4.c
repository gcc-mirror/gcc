/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse2" } */

_Float16 a;
__bf16 c;
_Complex _Float16 ac;

void
foo (_Float16* p)
{
  a = *p;
}

void
foo1 (__bf16 *p)
{
  c = *p;
}


void
foo2 (_Complex _Float16* p)
{
  ac = *p;
}
