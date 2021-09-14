// { dg-do compile { target c++20 } }
// { dg-additional-options -fmodules-ts }

module ;

#include <new>

export module foo;

export {
  struct A {
    alignas(std::hardware_destructive_interference_size) int x; // { dg-warning Winterference-size }
  };
}
