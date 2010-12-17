/* { dg-do compile } */
/* { dg-options "-fno-dse" } */

void bar(void);

void foo (int i, ...)
{
  __builtin_va_list ap;
  __builtin_va_start (ap, i);
  __builtin_va_arg (ap, int);
  while (i) i++;
  __builtin_va_arg (ap, int);
  while (i) i++;
  __builtin_va_arg (ap, int);
  while (i) i++;
  __builtin_va_arg (ap, int);
  if (i)
    bar ();
}

