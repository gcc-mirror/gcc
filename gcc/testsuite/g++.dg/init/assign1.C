// PR c++/13009
// { dg-do run }

struct A {
  char a;
};

struct B: public virtual A {
  #if 0 // this piece of code works around the problem
  B& operator= (const B& other)
  {
    A::operator= (other);
  }
  #endif
};

struct C: public B {
  char c;
};

int main() {
  B b;
  b.a = 'b';
  C c;
  c.a = c.c = 'c';
  
  c.B::operator= (b);
  if (c.a != 'b' || c.c != 'c')
    return 1;
}
