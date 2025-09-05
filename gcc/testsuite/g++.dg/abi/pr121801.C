// PR c++/121801
// { dg-do compile { target { c++20 && float16 } } }
// { dg-add-options float16 }

template<_Float16 T> void f() {}

void uses() {
  f<_Float16(1)>();
  f<_Float16(2)>();
}

// { dg-final { scan-assembler "_Z1fILDF16_3c00EEvv" } }
// { dg-final { scan-assembler "_Z1fILDF16_4000EEvv" } }
