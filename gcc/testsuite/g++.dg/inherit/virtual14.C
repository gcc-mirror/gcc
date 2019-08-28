// { dg-do run }

struct base 
{
  virtual operator int () { return 0;}
};

typedef int q;

struct d : base
{
  operator q () { return 1; }
};

int invoke (base *d)
{
  return int (*d);
}

int main ()
{
  d d;
  return !(invoke (&d) == 1);
}
