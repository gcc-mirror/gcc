typedef struct {
  long int p_x, p_y;
} Point;

int
f (Point basePt, Point pt1, Point pt2)
{
  long long vector;

  vector =
    (long long) (pt1.p_x - basePt.p_x) * (long long) (pt2.p_y - basePt.p_y) -
      (long long) (pt1.p_y - basePt.p_y) * (long long) (pt2.p_x - basePt.p_x);

  if (vector > (long long) 0)
    return 0;
  else if (vector < (long long) 0)
    return 1;
  else
    return 2;
}

main ()
{
  Point b, p1, p2;
  int answer;

  b.p_x = -23250;
  b.p_y = 23250;

  p1.p_x = 23250;
  p1.p_y = -23250;

  p2.p_x = -23250;
  p2.p_y = -23250;

  answer = f (b, p1, p2);

  if (answer != 1)
    abort ();
  exit (0);
}
