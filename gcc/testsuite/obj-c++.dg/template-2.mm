/* Test if ObjC classes (and pointers thereto) can participate
   in C++ overloading.  Correct handling of cv-qualifiers is 
   key here.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do compile } */

@interface foo {
  int a, b;
}
@end

struct bar {
  int c, d;
};

template <class _Tp>
struct allocator {
  typedef _Tp*       pointer;
  typedef const _Tp* const_pointer;
  typedef _Tp&       reference;
  typedef const _Tp& const_reference;

  pointer address(reference __x) const { return &__x; }
  const_pointer address(const_reference __x) const { return &__x; }
};

allocator<bar *> b;
allocator<foo *> d;
