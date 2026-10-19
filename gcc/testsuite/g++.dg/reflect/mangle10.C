// PR c++/126922
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection -O0 -fno-short-enums" }

struct A { decltype (^^::) a, b; };
struct B { A c; };

template <int N, auto C>
[[gnu::noipa]] void
foo ()
{
}

int
main ()
{
  foo <1, A { .a = {}, .b = {} }> ();
  foo <2, A { .a = {} }> ();
  foo <3, A { .b = {} }> ();
  foo <4, B { .c = {} }> ();
}

// { dg-final { scan-assembler "_Z3fooILi1ETnDaXtl1AEEEvv" } }
// { dg-final { scan-assembler "_Z3fooILi2ETnDaXtl1AEEEvv" } }
// { dg-final { scan-assembler "_Z3fooILi3ETnDaXtl1AEEEvv" } }
// { dg-final { scan-assembler "_Z3fooILi4ETnDaXtl1BEEEvv" } }
