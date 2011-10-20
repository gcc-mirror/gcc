// PR c++/17212
// { dg-options "-Wformat -Wno-format-zero-length" }

void f()
{
  __builtin_printf("");
}
