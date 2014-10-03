/* { dg-do compile { target { ilp32 || lp64 } } } */

struct w49
{
  union
  {
  }
  value;
};
void backtrace (const char *, int, int);
void
f9887 (struct w49 a23040)
{
  unsigned long r9887;
  if (((struct structure_type24753 *) (r9887 - 1)) == ((void *) 0))
    {
      backtrace ("stalin.sc", 7222, 248274);
    }
}
