// PR c++/26997
// { dg-do compile }
void * malloc (unsigned long size);
typedef struct { int a; } t;

void foo()
{
  t *v3;
  v3 = (t *)
    malloc(
	   sizeof(t) 
	   * 
           t->a // { dg-error "before '->' token" }
	   );
}

class C {
public:
  void operator[](int);
};

C bar (void)
{
  (C ())(3); // { dg-error "invalid cast" } 
  return (C ());
}

extern void baz (C,C);

void foo1 (void)
{
  baz ((C()), (C()));
}

struct S {
  void operator()(int);
};

int *var;
void foo2 (void)
{
  C ()[2];
  (C ())[2];
  (S ())(3); // { dg-error "invalid cast" } 
  (C())*var; // { dg-error "invalid cast" } 
  (C())+var;  // { dg-error "invalid cast" } 
  S()(3);
  (S()(3));
}

