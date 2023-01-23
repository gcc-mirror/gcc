// PR c++/107329

struct RexxClass {
  void *operator new(unsigned long, unsigned long, const char *, RexxClass *,
                     RexxClass *);
  void operator delete(void *, unsigned long, const char *, RexxClass *,
                       RexxClass *);
  RexxClass();
};
void createInstance() { new (sizeof(RexxClass), "", 0, 0) RexxClass; }
