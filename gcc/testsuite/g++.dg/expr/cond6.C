// { dg-do run }

extern "C" void abort ();
bool ok = false;

struct B {
  B() {}
  B(const B& b) { ok = true; }
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
  return !ok;
}
