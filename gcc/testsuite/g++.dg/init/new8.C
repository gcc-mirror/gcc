typedef __SIZE_TYPE__ size_t;

enum Refcount_Type {
  NO_REFCOUNT
};

struct d0_Unknown_Object
{
  void* operator new (size_t, size_t,  Refcount_Type type);
  void operator delete (void*, size_t, Refcount_Type);
  d0_Unknown_Object ();
};

void make ()
{
  new (10, NO_REFCOUNT) d0_Unknown_Object;
}
