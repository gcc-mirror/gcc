int glob;

conseq (a, b, c, d)
     int *a, *b;
{
  a[2] = d;
  a[1] = c;
  sequence (a, b, c, d);
  sequence (d, c, b, a);
  b[0] = 0;
  b[1] = 123;
  a[0] = 321;
  a[1] = 0;
  sequence (111, 0, 0, 222, 0, 333);
  ((int *)glob)[2] = c;
  ((int *)glob)[3] = d;
}
