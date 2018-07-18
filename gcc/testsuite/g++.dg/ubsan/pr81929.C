// PR sanitizer/81929
// { dg-do compile }
// { dg-options "-std=c++14 -fsanitize=undefined" }

struct S { S &operator<< (long); S foo (); S (); };

void
bar ()
{
  static_cast<S&>(S () << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0
		       << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0
		       << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0
		       << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0 << 0).foo ();
}
