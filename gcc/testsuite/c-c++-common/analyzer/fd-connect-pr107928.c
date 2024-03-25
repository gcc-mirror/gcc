struct sa {};

int
connect (int, struct sa *, int);

int
foo (struct sa sa)
{
  return connect (1, &sa, sizeof sa);
}
