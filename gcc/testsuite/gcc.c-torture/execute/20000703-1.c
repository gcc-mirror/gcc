void abort(void);
void exit(int);
struct baz 
{
  char a[17];
  char b[3];
  unsigned int c;
  unsigned int d;
};

void foo(struct baz *p, unsigned int c, unsigned int d)
{
  __builtin_memcpy (p->b, "abc", 3);
  p->c = c;
  p->d = d;
}

void bar(struct baz *p, unsigned int c, unsigned int d)
{
  ({ void *s = (p);
     __builtin_memset (s, '\0', sizeof (struct baz));
     s; });
  __builtin_memcpy (p->a, "01234567890123456", 17);
  __builtin_memcpy (p->b, "abc", 3);
  p->c = c;
  p->d = d;
}

int main()
{
  struct baz p;
  foo(&p, 71, 18);
  if (p.c != 71 || p.d != 18)
    abort();
  bar(&p, 59, 26);
  if (p.c != 59 || p.d != 26)
    abort();
  exit(0);
}
