// { dg-do assemble  }
// PRMS Id: 5341
// Bug: g++ complains about the explicit destructor notation.

#include <stddef.h>

void *operator new(size_t Size, void* pThing) { return pThing; }

template <class T> class Stack {
public:
  Stack() { new (Data) T(); }
  ~Stack() { ((T*)Data)->~T(); }
private:
  char Data[sizeof(T)];
};

Stack<int> a;
Stack<Stack<int> > c;
