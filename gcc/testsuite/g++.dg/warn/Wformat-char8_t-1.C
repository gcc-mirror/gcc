// PR c++/105626
// { dg-do compile { target c++11 } }
// { dg-options "-Wformat" }
// { dg-additional-options "-fchar8_t" { target c++17_down } }

int main()
{
  __builtin_printf((const char*) u8"test %d\n", 1); // { dg-bogus "format string" }
  return 0;
}
