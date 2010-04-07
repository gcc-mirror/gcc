// { dg-do assemble  }
typedef struct {
  int i;
} *p;

void f (p) { }			// { dg-error "with no linkage" }
p q;				// { dg-warning "with no linkage" }

int main()
{
  extern p j;			// { dg-warning "with no linkage" }
  struct A { int j; };
  extern A a;			// { dg-warning "with no linkage" }
  extern void f (A);		// { dg-error "with no linkage" }
}
