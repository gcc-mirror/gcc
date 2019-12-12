// PR c++/89599
// { dg-do compile { target c++11 } }

void foo (int x) {}
constexpr void *arr[2] = { (void*) &foo, (void *) foo };// { dg-error "'reinterpret_cast' is not a constant expression" }
constexpr void *ptr = (void *) &foo;			// { dg-error "'reinterpret_cast' is not a constant expression" }
