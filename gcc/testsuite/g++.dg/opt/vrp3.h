struct R
{
  long long r1, r2;
  void copy (R const &r) { r1 = r.r1; r2 = r.r2; }
  R ();
  explicit R (int, int);
  R (R const &r) { copy (r); }
  static int compare (R const &, R const &);
};
