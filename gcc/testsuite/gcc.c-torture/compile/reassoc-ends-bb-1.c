/* A call that can make an abnormal goto ends its basic block.  A range
   test on its result must not be inserted after it.  */

int rt (void) __attribute__ ((returns_twice));
_Bool cmp (void);

unsigned
f (void)
{
  unsigned s = 0;
  int m = 0;
  rt ();
  _Bool c = cmp ();
  for (int i = 0; i < 4; i++)
    {
      m = -(int) c;
      s = s * 5u + (m ? 0u : 3u);
    }
  return s + (m != -1 || c);
}
