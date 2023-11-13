void abort (void);
void exit (int);

typedef struct {
  long int p_x, p_y;
} Point;

void
bar ()
{
}

void
f (p0, p1, p2, p3, p4, p5)
     Point p0, p1, p2, p3, p4, p5;
{
  if (p0.p_x != 0 || p0.p_y != 1
      || p1.p_x != -1 || p1.p_y != 0
      || p2.p_x != 1 || p2.p_y != -1
      || p3.p_x != -1 || p3.p_y != 1
      || p4.p_x != 0 || p4.p_y != -1
      || p5.p_x != 1 || p5.p_y != 0)
    abort ();
}

void
foo ()
{
  Point p0, p1, p2, p3, p4, p5;

  bar();
  
  p0.p_x = 0;
  p0.p_y = 1;

  p1.p_x = -1;
  p1.p_y = 0;

  p2.p_x = 1;
  p2.p_y = -1;

  p3.p_x = -1;
  p3.p_y = 1;

  p4.p_x = 0;
  p4.p_y = -1;

  p5.p_x = 1;
  p5.p_y = 0;

  f (p0, p1, p2, p3, p4, p5);
}

int
main()
{
  foo();
  exit(0);
}
