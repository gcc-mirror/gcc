/* { dg-do compile } */

/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

extern double cos (double x);

class bend_class
{
  double *s_A;
public:
  void set_s_A (double s_A0)
  {
    s_A[0] = s_A0;
  }
};
class bend_set
{
  bend_class *bend_array;
public:
  void set_s_A (int index, double s_A0)
  {
    bend_array[index].set_s_A (s_A0);
  }
  void compute_s (void)
  {
    int i, j;
    double val;
    double tmp[3];
    for (i = 0; i < 5; ++i)
    {
      val = i;
      for (j = 0; j < 2; ++j)
        tmp[j] = cos (val);
      set_s_A (i, tmp[0]);
      tmp[j] = cos (val);
    }
  }
};
class internals
{
  bend_set bend;
  void compute_s (void);
};
void
internals::compute_s (void)
{
  bend.compute_s ();
}

