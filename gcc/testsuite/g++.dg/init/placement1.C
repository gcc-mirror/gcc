// PR c++/11266
// We were expanding the same TARGET_EXPR twice, for placement new and
// delete.

void* operator new    (__SIZE_TYPE__, void*) throw();
void  operator delete (void*, void*)        throw();

struct A { A(); };

void foo() { new(new A)A; }
