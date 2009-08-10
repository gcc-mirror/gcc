// { dg-do assemble  }
typedef struct {
  int i;
} *p;

void f (p) { }
p q;

int main()
{
  extern p j;			// { dg-error "anonymous type" }
  j+1;
  struct A { int j; };
  extern A a;			// { dg-error "local type" }
  a.j+1;
  extern void f (A);		// { dg-error "local type" }
  f(a);
}
