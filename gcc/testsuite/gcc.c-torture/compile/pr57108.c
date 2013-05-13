/* PR target/57108  */

void __assert_func (void) __attribute__ ((__noreturn__));

void
ATATransfer (int num, int buffer)
{
  int wordCount;

  while (num > 0)
    {
      wordCount = num * 512 / sizeof (int);

      ((0 == (buffer & 63)) ? (void)0 : __assert_func () );
      ((0 == (wordCount & 31)) ? (void)0 : __assert_func ());
    }
}
