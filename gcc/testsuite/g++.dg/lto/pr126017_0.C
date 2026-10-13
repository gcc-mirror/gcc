// PR c++/126017
// { dg-lto-do run }
// { dg-lto-options { { -std=c++26 -O2 -flto -freflection -g } } }

using info = decltype (^^::);
info a {};
extern info foo (info);

int
main ()
{
  if (a != info{} || foo (a) != a)
    __builtin_abort ();
}
