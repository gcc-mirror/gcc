/* PR c/3444
   This used to fail because bitwise xor was improperly computed in char type
   and sign extended to int type.  */

extern void abort ();
extern void exit (int);

signed char c = (signed char) 0xffffffff;

int foo (void)
{
  return (unsigned short) c ^ (signed char) 0x99999999;
}

int main (void)
{
  if ((unsigned char) -1 != 0xff
      || sizeof (short) != 2
      || sizeof (int) != 4)
    exit (0);
  if (foo () != (int) 0xffff0066)
    abort ();
  exit (0);
}
