void *calloc (__SIZE_TYPE__, __SIZE_TYPE__);

struct B
{
  B (short);
  int cls;
} k (0);

void d (int);

enum e {} i;

struct j
{
  void *operator new (__SIZE_TYPE__ b)
  {
    return calloc (b, sizeof (int)); // { dg-warning "leak" }
  }
  j (B *, int)
  {
  } // { dg-warning "leak" }
};

j *
f (B * b, int h, bool)
{
  d (b->cls);
  return new j (b, h); // { dg-warning "leak" }
}

void
m ()
{
  if (i)
    f (&k, 0, false);
}
