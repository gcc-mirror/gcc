/* Machine description pattern tests.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -dP" } */
/* { dg-do run { target { s390_useable_hw } } } */
/* Skip test if -O0 is present on the command line:

    { dg-skip-if "" { *-*-* } { "-O0" } { "" } }

   Skip test if the -O option is missing from the command line
    { dg-skip-if "" { *-*-* } { "*" } { "-O*" } }
*/

__attribute__ ((noinline))
unsigned int andc_vv(unsigned int a, unsigned int b)
{ return ~b & a; }
/* { dg-final { scan-assembler ":16 .\* \{\\*andsi3_\(esa\|zarch\)\}" } } */
/* { dg-final { scan-assembler ":16 .\* \{\\*xorsi3\}" } } */

__attribute__ ((noinline))
unsigned int andc_pv(unsigned int *a, unsigned int b)
{ return ~b & *a; }
/* { dg-final { scan-assembler ":22 .\* \{\\*andsi3_\(esa\|zarch\)\}" } } */
/* { dg-final { scan-assembler ":22 .\* \{\\*xorsi3\}" } } */

__attribute__ ((noinline))
unsigned int andc_vp(unsigned int a, unsigned int *b)
{ return ~*b & a; }
/* { dg-final { scan-assembler ":28 .\* \{\\*andsi3_\(esa\|zarch\)\}" } } */
/* { dg-final { scan-assembler ":28 .\* \{\\*xorsi3\}" } } */

__attribute__ ((noinline))
unsigned int andc_pp(unsigned int *a, unsigned int *b)
{ return ~*b & *a; }
/* { dg-final { scan-assembler ":34 .\* \{\\*andsi3_\(esa\|zarch\)\}" } } */
/* { dg-final { scan-assembler ":34 .\* \{\\*xorsi3\}" } } */

/* { dg-final { scan-assembler-times "\tnr\?k\?\t" 4 } } */
/* { dg-final { scan-assembler-times "\txr\?k\?\t" 4 } } */

int
main (void)
{
  unsigned int a = 0xc000000cu;
  unsigned int b = 0x5000000au;
  unsigned int e = 0x80000004u;
  unsigned int c;

  c = andc_vv (a, b);
  if (c != e)
    __builtin_abort ();
  c = andc_pv (&a, b);
  if (c != e)
    __builtin_abort ();
  c = andc_vp (a, &b);
  if (c != e)
    __builtin_abort ();
  c = andc_pp (&a, &b);
  if (c != e)
    __builtin_abort ();
  return 0;
}
