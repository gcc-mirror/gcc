// PR c++/15097
// { dg-do run }

typedef __SIZE_TYPE__ size_t;

extern "C" void * malloc (size_t);
extern "C" void free (void *);
extern "C" void abort(void);

void *saved;

void * operator new (size_t size)
{
  void *p = malloc (size);
  saved = p;
  return p;
}

void operator delete (void *p)
{
  if (p != saved)
    abort ();
  free (p);
}

struct B1
{
    virtual ~B1 () throw() {}
    B1 (){}
    int x;
};
struct B2
{
    virtual ~B2 () throw() {}
    B2 (){}
    int x;
};
struct D : B1, B2
{
    D (){}
    ~D () throw() {}
    int y;
};
void f1 (D*);
void f2 (B2*);
void f3 (B1*);
int main (void)
{
    f1 (::new D);
    f2 (::new D);     
    f3 (::new D);
}
void f1 ( D* p) { ::delete p; }
void f2 (B2* p) { ::delete p; }  
void f3 (B1* p) { ::delete p; }
