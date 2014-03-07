// PR c++/51265
// { dg-do compile { target c++11 } }

struct Funny
{
  int print(int);
};

template<typename X>
void c();

template<typename X, X ff>
void xx()
{
  c<decltype(ff)>();
}

int main()
{
  xx<int(Funny::*)(int), &Funny::print>();
}
