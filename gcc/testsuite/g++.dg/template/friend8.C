template <int N> struct ivector
{
  template <int r, int c> friend void
    mult_mv ();
};

template struct ivector<3>;

template <int r, int c> void
mult_mv ()
{
  c;
}

void get_local_point_pos ()
{
  mult_mv<7, 3> ();
}
