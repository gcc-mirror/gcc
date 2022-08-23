/* Reduced from pr100244.C.  */
inline void *operator new (__SIZE_TYPE__, void *__p) { return __p; }

struct int_container {
  int i;
  int *addr () { return &i; }
};

struct int_and_addr {
  int i;
  int *addr;
  int_and_addr () { addr = &i; } /* { dg-warning "overflow" } */
};

int test (int_container ic)
{
  int_and_addr *iaddr = new (ic.addr ()) int_and_addr;
  return iaddr->i;
}
