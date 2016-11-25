/* PR target/78438 */

char a = 0;
int b = 197412621;

__attribute__ ((noinline, noclone))
void foo ()
{
  a = 0 > (short) (b >> 11);
}

int
main ()
{
  asm volatile ("" : : : "memory");
  if (__CHAR_BIT__ != 8 || sizeof (short) != 2 || sizeof (int) < 4)
    return 0;
  foo ();
  if (a != 0)
    __builtin_abort ();
  return 0;
}
