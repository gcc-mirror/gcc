// PR c++/88183
// { dg-do compile { target c++17 } }

struct A { int i; };

template <class T> T& g(T);

template <class U, class... Vs>
void f(U u, Vs... vs)
{ 
  [vs...](auto x) {
    (g(x) .* ... .* vs) = 42;
  }(u);
}

int main()
{
  f(A(), &A::i);
}
