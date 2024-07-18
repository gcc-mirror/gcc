/* { dg-do run } */

typedef struct {
  char hours, day, month;
  short year;
} T;

T g (void)
{
  T now;
  now.hours = 1;
  now.day = 2;
  now.month = 3;
  now.year = 4;
  return now;
}

__attribute__((const)) T f (void)
{
  T virk = g ();
  return virk;
}

int main ()
{
  if (f ().hours != 1 || f ().day != 2 || f ().month != 3 || f ().year != 4)
    __builtin_abort ();
  return 0;
}
