/* { dg-do compile } */
/* { dg-additional-options "-Wno-stringop-overread" } */

int foo (fmt)
char* fmt;
{
  return (__builtin_strchr (fmt, '*') != 0
          || __builtin_strchr (fmt, 'n') != 0);
}
void bar ()
{
  if (foo (1))
    __builtin_abort ();
}
