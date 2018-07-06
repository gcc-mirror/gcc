/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc_elfv2 } } */
/* { dg-options "-O2" } */

/* This used to generate a rotate:DI by 44, with mask 0xf00, which is
   implemented using a rlwinm instruction.  We used to write 44 for the
   shift count there; it should be 12.  */

struct A
{
  int a : 4;
  int : 2;
  int b : 2;
  int : 2;
  int c : 2;
  int d : 1;
  int e;
};
struct B
{
  int a : 4;
} *a;
void bar (struct A);

void
foo (void)
{
  struct B b = a[0];
  struct A c;
  c.a = b.a;
  c.b = 1;
  c.c = 1;
  c.d = 0;
  bar (c);
}

/* { dg-final { scan-assembler-not {(?n)rlwinm.*,44,20,23} } } */
