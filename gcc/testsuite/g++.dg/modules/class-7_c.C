import One;
import Two;

struct derived : middle
{
  long long d;

  derived (int b_, int m_, int d_)
    : middle (b_, m_), d (d_)
  {
  }
};

int check (derived *d)
{
  if ((char *)&d->b != (char *)&d->d + sizeof (long long))
    return 3;
  if ((char *)&d->d != (char *)&d->m + sizeof (long long))
    return 4;
  return 0;
}


int main ()
{
  middle m (1, 2);


  if (m.b != 1 || m.m != 2)
    return 1;

  derived d (1, 2, 3);
  
  if (d.b != 99 || d.m != 2 || d.d != 3)
    return 2;

  return check (&d);
}
