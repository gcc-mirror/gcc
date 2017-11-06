// { dg-do run { target c++11 } }

extern "C" void abort();

template <class T>
auto apply (T t) -> decltype (t())
{
  return t();
}

template <class T>
void f(T t)
{
  T t2 = t;
  if (t != [=]()->T { return t; }())
    abort ();
  if (t != [=] { return t; }())
    abort ();
  if (t != [=] { return t2; }())
    abort ();
  if (t != [&] { return t; }())
    abort ();
  if (t != apply([=]{return t;}))
    abort ();

  int i;
  [&] (int a)                    { return a+i+t; } (0);
  [&] (int a) -> decltype(a)     { return a+i+t; } (0);
  [&] (int a) -> decltype(i)     { return a+i+t; } (0);
  [&] (int a) -> decltype(t)     { return a+i+t; } (0);
  [&] (int a) -> decltype(a+i)   { return a+i+t; } (0);
  [&] (int a) -> decltype(a+t)   { return a+i+t; } (0);
  [&] (int a) -> decltype(i+t)   { return a+i+t; } (0);
  [&] (int a) -> decltype(a+i+t) { return a+i+t; } (0);
}

int main()
{
  f(0xbeef);
}
