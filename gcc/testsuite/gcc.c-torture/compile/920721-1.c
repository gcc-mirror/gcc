typedef struct{short ttype;float s;}T;
short t[8][8];

T f(T t2,T t1)
{
  T x;
  if (t1.ttype == 1)
    x.ttype = t[t2.ttype][t1.ttype],
    x.s = 1;
  return x;
}
