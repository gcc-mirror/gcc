// PR c++/32596
// { dg-do compile }

namespace
{
  template<class T> inline void char_less(void) { }
  template<> inline void char_less<char>(void) { }
}
