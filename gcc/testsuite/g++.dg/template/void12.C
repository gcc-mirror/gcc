// PR c++/28639

template<void> struct A  // { dg-error "not a valid type" }
{
  static const int i = 1;
  char a[i];
};
