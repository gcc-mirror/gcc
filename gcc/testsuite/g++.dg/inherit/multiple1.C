// { dg-options "-w" }

struct Base {
  int b;
  
  Base(int b) : b(b) { }
};

struct Derived : public Base {
  Derived(int d) : Base(d) { }
};

struct Final : public Derived, public Base {
  Final(int f) : Derived(f), Base(f-1) { }
};

int main()
{
  Final f(5);
}
