/* PR middle-end/98160: bogus -Wfree-nonheap-object calling member delete
   on the result of inline member new plus offset
   { dg-do compile }
   { dg-options "-O2" } */

struct MemoryManager { void* allocate (); };

struct XMemory
{
  void* operator new (__SIZE_TYPE__, MemoryManager *mgr)
  {
    void *p = mgr->allocate ();
    return (char*)p + sizeof(MemoryManager);
  }

  void operator delete (void*, MemoryManager*);
};

struct XMLMutex: XMemory {
  XMLMutex();
};

void gValidatorMutex (MemoryManager *mgr)
{
  new (mgr) XMLMutex;   // { dg-bogus "\\\[-Wfree-nonheap-object" }
}
