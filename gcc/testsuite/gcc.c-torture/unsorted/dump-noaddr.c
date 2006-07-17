#if MASK & 1
#define t16(x) x x x x x x x x x x x x x x x x
#define M (sizeof (t16(t16(t16(t16(t16(" ")))))) - 1)
#endif
#if MASK & 2
#define M 1048576
#endif

typedef struct s {
  int c;
  void *vp;
  struct s *s;
}s;

typedef int (*fpt) (const char *, void *, int *);

int M_var = M;

extern void exit (int);

int
f (int start, int end, int *a, int *b, int c, s *sp)
{
  int count = 0;
  int i;

  for (i = start; i <= end; i++)
    {
      a[i] = b[i] + c;
      count ++;
    }
  (*(fpt)sp->s->vp) ("Hello World!\n", &exit, &M_var);
  return count;
}

int
g (int i)
{
  switch (i)
    {
    case 1: return 42;
    case 2: return 60;
    case 3: return 7;
    case 4: return 3;
    case 5: return M;
    default: return 0;
    }
}
