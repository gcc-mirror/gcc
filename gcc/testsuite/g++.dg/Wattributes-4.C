// PR c++/83322 - ICE: tree check: expected class ‘type’, have ‘exceptional’
// (baselink) in diag_attr_exclusions, at attribs.c:393
// { dg-do compile }
// { dg-options "-Wattributes" }

#define ATTR(list) __attribute__ (list)

// Test case from comment #0.
struct A0
{
  template <class T> operator T();
  ATTR ((always_inline)) operator int();
};

// Test case from comment #4.
struct A1
{
  void foo();
};

struct B
{
  bool foo;
};

struct C: A1, B
{
  ATTR ((warn_unused_result)) int foo ();
};
