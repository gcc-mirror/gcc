// { dg-do compile }
// { dg-require-effective-target lto }
// { dg-options "-flto -g" }

template < typename > void *
bar (int *p)
{
  union
    {
      int *p;
    }
  u;
  u.p = p;
  return u.p;
}

void
foo (int *p)
{
  bar < void >(p);
}
