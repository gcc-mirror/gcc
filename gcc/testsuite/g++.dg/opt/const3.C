// PR optimization/12926
// This failed on SPARC64 because the assignments to the bit-fields
// were wrongly swapped in the constructor.

// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort(void);

typedef __SIZE_TYPE__ size_t;

void *my_out;

struct A
{
  enum Type {P, U, S};

  int foo1(void *, const char *);
  int foo2(int, const Type);

  A (const size_t size, const Type type): mSize(size), mType(type)
  {
      foo2(foo1(my_out, "type = "), type);
      foo2(foo1(my_out, "mType = "), mType);
  }

  const size_t mSize : 8*sizeof(size_t) - 3;
  Type mType : 2;
};

int i;

int A::foo1(void *ios, const char *str) { return 0; }
int A::foo2(int v, const Type t) { i=0; return 0; }

int main()
{
  A testa(2, A::S);

  if (testa.mType != A::S)
     abort();

  return 0;
}
