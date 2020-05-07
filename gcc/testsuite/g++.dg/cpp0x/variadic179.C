// PR c++/94628
// { dg-do compile { target c++11 } }

int f(int, int);
int f(int);

template<class...Args>
auto select(Args... args) -> decltype(f(args...))
{
  if (sizeof...(Args) > 1)
    return select<char>(7);
  else
    return 0;
}

int a = select(0, 1);
