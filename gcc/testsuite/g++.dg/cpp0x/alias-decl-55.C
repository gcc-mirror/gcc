// PR c++/71718
// { dg-do compile { target c++11 } }

template <typename T>
class A : T{};

template <typename T>
using sp = A<T>;

struct Base {};

template <typename T, int num = 1>
const sp<T>
rec() 			// { dg-error "depth" }
{
  return rec<T, num - 1>();  
}

static void f(void) {
  rec<Base>();
}

// { dg-prune-output "compilation terminated" }
