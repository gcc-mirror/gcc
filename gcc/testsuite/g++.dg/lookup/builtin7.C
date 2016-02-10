// PR c++/69657

typedef unsigned int size_t;
namespace std {
extern void *calloc(size_t, size_t);
}
using std::calloc;
int
main ()
{
  char *(*pfn) = (char *(*)) calloc ;
  (bool)&calloc;
  (bool)&::calloc;
}
