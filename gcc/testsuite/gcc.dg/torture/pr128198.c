/* { dg-do run } */

typedef struct Vec { unsigned char *start, *finish; } Vec;

unsigned long size = 1;  /* not a constant, to keep IPA-CP away */
static long freed;

static void create_storage(Vec *v, unsigned long n)
{
  unsigned char *p = 0;
  if (n)
    p = (unsigned char *)__builtin_malloc(n);
  v->start = p;
  v->finish = p ? p + n : p;
}

static void destroy(Vec *v)
{
  if (v->start)
  {
    freed += (unsigned long)v->finish - (unsigned long)v->start;
    __builtin_free(v->start);
    v->start = 0;
    v->finish = nullptr;
  }
}

static void assign(Vec *v, unsigned long n)
{
  if (n > (unsigned long)v->finish - (unsigned long)v->start)
  {
    Vec tmp = {0, 0};
    create_storage(&tmp, n);
    unsigned char *s = tmp.start, *f = tmp.finish;
    tmp.start = v->start; tmp.finish = v->finish;
    v->start = s; v->finish = f;
    destroy(&tmp);
  }
}

int main ()
{
  Vec fixed[4] = {{0, 0}};
  for (Vec *c = fixed; c != fixed + 4; ++c)
    assign(c, size);
  for (Vec *c = fixed; c != fixed + 4; ++c)
  {
    int t = 0;
    for (unsigned char *b = c->start,
         *e = c->finish; b != e; ++b)
    {
      t++;
      if (t >= 20)
        __builtin_trap();
    }
  }
  for (Vec *c = fixed; c != fixed + 4; ++c)
    destroy(c);
  return 0;
}
