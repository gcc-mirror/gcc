// PR c++/55081
// { dg-do compile }

struct R { int field; } r;

__UINTPTR_TYPE__ *
foo ()
{
  static __UINTPTR_TYPE__ array[] = {
    sizeof (char),
    (reinterpret_cast <__UINTPTR_TYPE__>(&r.field)
     - reinterpret_cast <__UINTPTR_TYPE__>(&r)) + 1
  };
  return array;
}

// { dg-final { scan-assembler-not "_ZGVZ3foovE5array" } }
