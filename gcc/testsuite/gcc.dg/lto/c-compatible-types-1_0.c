/* { dg-lto-do run } */
/* { dg-lto-options { {-O3 -fcommon} {-fcommon} } } */

/* By C standard Each enumerated type shall be compatible with char, a  signed
   integer, type, or an unsigned integer type. The choice of type is
   implementation-defined.  Check that enum and unsigned int match.  */
unsigned int a;
unsigned int *b;
void t();

void reset ()
{
  asm("":"=r"(a):"0"(0));
}
int
main()
{
  asm("":"=r"(a):"0"(1));
  asm("":"=r"(b):"0"(&a));
  t();
  return 0;
}
