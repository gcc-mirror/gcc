/* Verify that we either do not do any devirtualization or correctly
   spot that foo changes the dynamic type of the passed object.  */

/* { dg-do run } */
/* { dg-options "-O3"  } */

extern "C" void abort (void);
extern "C" void *malloc(__SIZE_TYPE__);

inline void* operator new(__SIZE_TYPE__, void* __p) throw() { return __p;}

int x;

class A {
public:
   virtual ~A() { }
};

class B : public A {
public:
   virtual ~B() { if (x == 1) abort (); x = 1; }
};

void __attribute__((noinline,noclone)) foo (void *p)
{
 B *b = reinterpret_cast<B *>(p);
 b->~B();
 new (p) A;
}

int main()
{
 void *p = __builtin_malloc (sizeof (B));
 new (p) B;
 foo(p);
 reinterpret_cast<A *>(p)->~A();
 return 0;
}
