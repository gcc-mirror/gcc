// PR c++/107329

typedef __SIZE_TYPE__ size_t;
struct RexxClass {
  void *operator new(size_t, size_t, const char *, RexxClass *,
                     RexxClass *);
  void operator delete(void *, size_t, const char *, RexxClass *,
                       RexxClass *);
  RexxClass();
};
void createInstance() { new (sizeof(RexxClass), "", 0, 0) RexxClass; }
