// Bug: We were calling f() twice, for both the placement new and placement
// delete calls.

// { dg-do run }

void* operator new    (__SIZE_TYPE__ sz, void*) { return operator new (sz); }
void  operator delete (void* p, void*)         { operator delete (p); }

struct A { A() { throw 1; } };

int c;
void *f() { ++c; return 0; }

int main()
{
  try
    {
      new (f()) A;
    }
  catch (...) {}
  return c != 1;
}
