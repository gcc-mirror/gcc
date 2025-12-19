// PR debug/122968
// { dg-do run { target int32plus } }
// { dg-options "-g" }

struct S { unsigned a; unsigned long long b; short c; } s = { 10, 53967718, -42 };
auto [ a, b, c ] = s;
static auto [ d, e, f ] = S { 5, 1245234412, -231 };
auto *p = &d;
volatile int v;
namespace N {
  auto [ a, b, c ] = S { 8, 7135498, 256 };
  int arr[4] = { 1, 2, 3, 4 };
  auto [ d, e, f, g ] = arr;
}

int
main ()
{
  asm volatile ("" : : "g" (&a), "g" (&b), "g" (&c) : "memory");
  asm volatile ("" : : "g" (&d), "g" (&e), "g" (&f) : "memory");
  asm volatile ("" : : "g" (&N::a), "g" (&N::b), "g" (&N::c) : "memory");
  asm volatile ("" : : "g" (&N::d), "g" (&N::e), "g" (&N::f) : "memory");
  asm volatile ("" : : "g" (&N::g) : "memory");
  v = 1;
// { dg-final { gdb-test 24 "a" "10" } }
// { dg-final { gdb-test 24 "b" "53967718" } }
// { dg-final { gdb-test 24 "c" "-42" } }
// { dg-final { gdb-test 24 "d" "5" } }
// { dg-final { gdb-test 24 "e" "1245234412" } }
// { dg-final { gdb-test 24 "f" "-231" } }
// { dg-final { gdb-test 24 "N::a" "8" } }
// { dg-final { gdb-test 24 "N::b" "7135498" } }
// { dg-final { gdb-test 24 "N::c" "256" } }
// { dg-final { gdb-test 24 "N::d" "1" } }
// { dg-final { gdb-test 24 "N::e" "2" } }
// { dg-final { gdb-test 24 "N::f" "3" } }
// { dg-final { gdb-test 24 "N::g" "4" } }
}
