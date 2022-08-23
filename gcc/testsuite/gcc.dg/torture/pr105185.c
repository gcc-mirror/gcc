/* { dg-do compile } */

int foo (fmt)
char* fmt;
{
  return (__builtin_strchr (fmt, '*') != 0
          || __builtin_strchr (fmt, 'n') != 0);
}
void bar ()
{
  if (foo ())
    __builtin_abort ();
}
