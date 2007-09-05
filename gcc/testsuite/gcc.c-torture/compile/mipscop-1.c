/* { dg-do compile { target mips*-*-* } } */

register unsigned int cp0count asm ("$c0r1");

int __attribute__ ((nomips16))
main (int argc, char *argv[])
{
  unsigned int d;

  d = cp0count + 3;
  printf ("%d\n", d);
}
