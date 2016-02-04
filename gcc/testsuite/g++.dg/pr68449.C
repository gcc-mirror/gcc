// PR c++/68449
// { dg-do compile }
// { dg-options "-Wsign-compare" }

int
foo (int a)
{
  return __extension__ ({ int b; b; }) < 0;
}
