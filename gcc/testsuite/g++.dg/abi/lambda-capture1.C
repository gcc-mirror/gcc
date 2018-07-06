// PR c++/84726
// { dg-do compile { target c++11 } }

#define SA(X) static_assert (X, #X)

int main()
{ 
  const int i = 42;
  auto l = [=]{return i+i;};
  SA(sizeof(l) == 1);
}
