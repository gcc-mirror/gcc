// { dg-do run { target i?86-*-* } }
// { dg-options "-fabi-version=0 -w" }

struct S {
  virtual void f() {}
};

struct T : virtual public S { };

struct U : public S, virtual public T { 
  char c[100];
};

struct V : public U, virtual public S {};

struct W : public V {
  int i;
};

int main () {
  W w;

  if ((char*) &w.i - (char *) &w != 104)
    return 1;
}

