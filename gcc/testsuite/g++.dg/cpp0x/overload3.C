// PR c++/59823
// { dg-do compile { target c++11 } }

struct X { };

void f(X&&);

struct wrap
{
  operator const X&() const;
};

int main()
{
  wrap w;
  f(w);				// { dg-error "" }
}
