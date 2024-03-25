// PR c++/34158

// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.

typedef __SIZE_TYPE__ size_t;
extern "C" void* malloc (size_t);
extern "C" void free (void *);

template <class T> class undef;

struct A {
  A() { throw 1; }
};

template<typename T> class Pool { };

void *operator new(size_t size,Pool<int>& pool)
{
  return malloc(size);
}

template<typename T>
void operator delete(void *p,Pool<T>& pool)
{
  undef<T> t;			// { dg-error "incomplete" }
  free(p);
}

int main ()
{
  Pool<int> pool;
  new (pool) A();		// { dg-message "required" }
  return 0;
}
