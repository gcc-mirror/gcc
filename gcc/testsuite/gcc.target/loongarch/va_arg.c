/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Technically we shouldn't save any register for this function: it should be
   compiled as if it accepts 3 named arguments.  But AFAIK no compilers can
   achieve this "perfect" optimization now, so just ensure we are using the
   knowledge provided by stdarg pass and we won't save GARs impossible to be
   accessed with __builtin_va_arg () when the va_list does not escape.  */

/* { dg-final { scan-assembler-not "st.*r7" } } */

int
test (int a0, ...)
{
  void *arg;
  int a1, a2;

  __builtin_va_start (arg, a0);
  a1 = __builtin_va_arg (arg, int);
  a2 = __builtin_va_arg (arg, int);
  __builtin_va_end (arg);

  return a0 + a1 + a2;
}
