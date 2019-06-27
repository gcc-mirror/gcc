// { dg-do compile }
// { dg-additional-options "-g" }

struct cb {
    int yr;
};

void *
operator new (__SIZE_TYPE__, void *nq)
{
  return nq;
}

void
af (int xn)
{
  new (&xn) cb { };
}
