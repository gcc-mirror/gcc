/* { dg-do run } */
/* { dg-options "-O -fschedule-insns" } */

int f4 (int i, ...)
{
  int y = 0;
   __builtin_va_list ap;
   __builtin_va_start(ap, i);
   if (i == 5) y = __builtin_va_arg(ap, double);
   __builtin_va_end(ap);
   return y;
}

int main (void)
{
  if (f4 (5, 7.0) != 7)
    __builtin_abort ();
  return 0;
}
