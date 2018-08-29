// PR middle-end/70100
// { dg-do compile { target c++11 } }
// { dg-options "-O0" }

void
bar (int)
{
}

template <typename ... Args>
void
foo (Args && ... args)
{
  [&] { [&] { bar(args...); }; };
}

int
main ()
{
  foo (2);
}
