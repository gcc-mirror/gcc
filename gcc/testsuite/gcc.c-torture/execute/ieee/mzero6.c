/* Tail call optimizations would convert func() into the moral equivalent of:

       double acc = 0.0;
       for (int i = 0; i <= n; i++)
	 acc += d;
       return acc;

   which mishandles the case where 'd' is -0.  They also initialised 'acc'
   to a zero int rather than a zero double.  */

void abort (void);
void exit (int);

double func (double d, int n)
{
  if (n == 0)
    return d;
  else
    return d + func (d, n - 1);
}

int main ()
{
  if (__builtin_copysign (1.0, func (0.0 / -5.0, 10)) != -1.0)
    abort ();
  exit (0);
}
