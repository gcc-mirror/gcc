// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 18 Jan 2001 <nathan@codesourcery.com>

// Bug 1617. We didn't resolve partial ordering properly. The std is rather
// vague about it anyway, DR 214 talks about this.

extern "C" int puts (char const *);

template <typename T> int Foo (T *) {puts (__PRETTY_FUNCTION__); return 1;}
template <typename T> int Foo (T &) {puts (__PRETTY_FUNCTION__); return 2;}
template <typename T> int Foo (T const &) {puts (__PRETTY_FUNCTION__); return 3;}

template <typename T> int Bar (T const *const &) {puts (__PRETTY_FUNCTION__); return 4;}
template <typename T> int Bar (T *const &) {puts (__PRETTY_FUNCTION__); return 5;}
template <typename T> int Bar (T *) {puts (__PRETTY_FUNCTION__); return 6;}

template <typename T> int Quux (T *const &) {puts (__PRETTY_FUNCTION__); return 7;}
template <typename T> int Quux (T const &) {puts (__PRETTY_FUNCTION__); return 8;}


int Baz (int const *ptr, int *ptr2)
{
  if (Foo (ptr) != 1)
    return 1;
  if (Foo (ptr2) != 1)
    return 2;
  if (Foo (*ptr) != 3)
    return 3;
  if (Foo (*ptr2) != 2)
    return 4;
  
  if (Bar (ptr) != 4)
    return 5;
  
  if (Quux (ptr) != 7)
    return 5;
  if (Quux (ptr2) != 7)
    return 6;
  
  return 0;
}

int main ()
{
  return Baz (0, 0);
}
