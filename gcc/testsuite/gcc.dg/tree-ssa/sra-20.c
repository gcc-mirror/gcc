/* { dg-do compile } */
/* { dg-options "-O1 -Wall" } */
/* PR/70013, SRA of constant-pool loads removes initialization of part of d.  */
#pragma pack (1)
struct S0 {
  unsigned f0 : 17;
};

int c;

int
main (int argc, char **argv)
{
  struct S0 d[] = { { 1 }, { 2 } };
  struct S0 e = d[1];

  c = d[0].f0;
  __builtin_printf ("%x\n", e.f0);
  return 0;
}
