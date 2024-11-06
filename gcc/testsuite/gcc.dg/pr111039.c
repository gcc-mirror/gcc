/* { dg-do compile } */
/* { dg-options "-std=gnu17 -O" } */

#if __SIZEOF_INT__ < 4
#define int __INT32_TYPE__
#endif

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
