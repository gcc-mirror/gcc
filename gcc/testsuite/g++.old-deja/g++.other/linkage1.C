typedef struct {
  int i;
} *p;

void f (p) { }			// ERROR - function uses anonymous type
p q;

int main()
{
  extern p j;
  struct A { int j; };
  extern A a;			// ERROR - extern uses local type
  extern void f (A);		// ERROR - extern uses local type
}
