struct VEC_char_base
{
  unsigned num;
  unsigned alloc;
  union {
      short vec[1];
      struct {
	  int i;
	  int j;
	  int k;
      } a;
  } u;
};

short __attribute__((noinline))
foo (struct VEC_char_base *p, int i)
{
  short *q;
  p->u.vec[i] = 0;
  q = &p->u.vec[16];
  *q = 1;
  return p->u.vec[i];
}

extern void abort (void);
extern void *malloc (__SIZE_TYPE__);

int
main()
{
  struct VEC_char_base *p = malloc (sizeof (struct VEC_char_base) + 256);
  if (foo (p, 16) != 1)
    abort ();
  return 0;
}
