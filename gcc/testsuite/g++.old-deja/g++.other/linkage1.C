// { dg-do assemble  }
typedef struct {
  int i;
} *p;

void f (p) { }			// { dg-error "" } function uses anonymous type
p q;

int main()
{
  extern p j;
  struct A { int j; };
  extern A a;			// { dg-error "" } extern uses local type
  extern void f (A);		// { dg-error "" } extern uses local type
}
