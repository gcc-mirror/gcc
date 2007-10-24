// PR c++/25950

struct X {
  X();
  explicit X(const X&);
};

void g(const X&);

int main()
{
  g(X());
}
