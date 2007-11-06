// PR target/33168
// { dg-do compile }
// { dg-options "-O2 -fdata-sections" }

extern const int& foo;
namespace
{
  const int bar = 16;
}
const int &foo = bar;
