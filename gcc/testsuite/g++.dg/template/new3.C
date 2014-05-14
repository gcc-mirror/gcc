extern void *operator new(__SIZE_TYPE__); // { dg-message "note" }

template <class T >
struct C
{
  void f() {
    int* node;
    new (&node) int(0); // { dg-error "new" }
  }
};

void* operator new(__SIZE_TYPE__, void* __p);

void g() {
  C<int> c;
  c.f();
} 
