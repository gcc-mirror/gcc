// PR c++/91548 - fix detecting modifying const objects for ARRAY_REF.
// { dg-do compile { target c++14 } }

constexpr int& impl(const int (&array)[10], int index) {
  return const_cast<int&>(array[index]);
}

struct A {
  constexpr int& operator[](int i) { return impl(elems, i); }
  int elems[10];
};

constexpr bool
f()
{
  A arr = {};
  arr[2] = true;
  return false;
}

constexpr bool b = f();
