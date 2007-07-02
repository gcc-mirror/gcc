void sub0(int);
void sub1(int *);
void sub2(int, long);
void sub3(int *, int *);
void sub4(int *, int *);

int main(int argc, char **argv)
{
  int i = 1;
  long j = 1;

  sub0(i);
  sub1(&i);
  sub2(i, j);
  sub3(&i, &i);
  sub4(&i, &i);

  return 0;
}
