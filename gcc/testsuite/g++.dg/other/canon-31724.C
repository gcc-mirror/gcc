struct ac {ac();};
ac  spline_rep1(void)
{
  typedef ac at[2];
  ac * b = new ac[2];
  at *a =  (at*)b;
  return (*a)[0];
}
