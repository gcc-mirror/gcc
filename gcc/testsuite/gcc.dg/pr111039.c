/* { dg-do compile } */
/* { dg-options "-O" } */

int _setjmp ();
void abcd ();
void abcde ();
void compiler_corruption_function(int flags)
{
  int nowait = flags & 1048576, isexpand = flags & 8388608;
  abcd();
  _setjmp(flags);
  if (nowait && isexpand)
    flags &= 0;
  abcde();
}
