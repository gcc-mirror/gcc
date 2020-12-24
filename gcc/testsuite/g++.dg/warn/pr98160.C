/* PR middle-end/98160 - ICE in warn_dealloc_offset on member placement
   new and delete
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

void* (*pf) (size_t);

struct A;
struct B
{
  B ();

  void* operator new (size_t, A*);
  void operator delete (void*, A*);
};

void operator delete (void *, A*);

void B::operator delete (void*, A *p)
{
  void *q = pf (1);
  ::operator delete ((char*)q + 1, p);
}

void* f (A *p)
{
  return new (p) B;
}
