void foo ();

void update (double* r)
{
  foo ();
    {
      register double  y1;
      y1 = r[ 4] - r[11];
    }
}
