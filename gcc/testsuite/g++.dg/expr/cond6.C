// { dg-do run }

extern "C" void abort ();

struct B {
  B() {}
  B(const B& b) { abort (); }
};

struct D : public B {
  D() {}
  D(const D& d) : B() {}
};

D d;
B b;

D f() {
  return d;
}

int main () {
  b = (true ? f() : b);
}
