struct sa {};

int
bind (int, struct sa *, int);

int
foo (struct sa sa)
{
  return bind (1, &sa, sizeof sa);
}
