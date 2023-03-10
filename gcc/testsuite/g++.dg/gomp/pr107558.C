// PR c++/107558
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fmerge-all-constants" }
// { dg-additional-options "-flto" { target lto } }

int a = 15;

void
foo ()
{
  auto &&l = [&]() { return a; };
#pragma omp target parallel
  l ();
}
