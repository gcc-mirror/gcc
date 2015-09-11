/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

typedef __SIZE_TYPE__ size_t;
inline void* operator new(size_t, void* p) throw() { return p; }

typedef void (*handler_t)(void);
extern handler_t get_handle();

inline void* operator new(size_t sz)
{
  void *p;

  if (sz == 0)
    sz = 1;

  while ((p = __builtin_malloc (sz)) == 0)
    {
      handler_t handler = get_handle ();
      if (! handler)
        throw 42;
      handler ();
    }
  return p;
}

struct vect {
  int *start, *end;
  vect(size_t n) {
    start = end = 0;
    if (n > (size_t)-1 / sizeof(int))
      throw 33;
    if (n != 0)
      start = static_cast<int*> (operator new (n * sizeof(int)));
    end = start + n;
    int *p = start;
    for (size_t l = n; l > 0; --l, ++p)
      *p = 0;
  }
};

void f (void *p, int n)
{
  new (p) vect(n);
}

/* { dg-final { scan-tree-dump-times "calloc" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "malloc" "optimized" } } */
/* { dg-final { scan-tree-dump-not "memset" "optimized" } } */
