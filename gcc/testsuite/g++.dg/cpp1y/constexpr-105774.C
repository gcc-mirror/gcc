// PR c++/105774
// { dg-do compile { target c++14 } }

constexpr signed char
foo ()
{
#if __SCHAR_MAX__ < __INT_MAX__
  signed char x = __SCHAR_MAX__;
#else
  signed char x = 0;
#endif
  return ++x;
}

constexpr auto a = foo ();
