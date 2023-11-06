typedef struct
  {
    long i;
    double f;
  } T;

extern int g(T *);

int
f (T *n1, T *n2)
{
  if (g (n2))
    return n1->i - n2->i;
  else
    {
      double f = n1->f - n2->i;
      return f == 0.0 ? 0 : (f > 0.0 ? 1 : -1);
    }
}
