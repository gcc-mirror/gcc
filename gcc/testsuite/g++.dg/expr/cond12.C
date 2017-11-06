// PR c++/58714
// { dg-do run }

struct X {
    X& operator=(const X&);
    X& operator=(X&){__builtin_abort();}
};

X g;
X& X::operator=(const X&)
{
  return g;
}

int main(int argv,char**) {
  X a, b;
  ((argv > 2) ? a : b) = X();
}
