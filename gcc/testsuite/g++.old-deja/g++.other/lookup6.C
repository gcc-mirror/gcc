// { dg-do run  }
int f(int i)
{
  struct C {
    int i;
    C () : i(1) {}
    int f() {    
      struct D {
        int i;
	D () : i(2) {}
        int g() { return i; }
      } d;

      return d.g();
    }
  } c;

  return c.f();
}


int main()
{
  if (f(0) != 2)
    return 1;
}
