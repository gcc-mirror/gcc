struct S
{
  int a:3;
  unsigned b:1, c:28;
};

struct S x = {1, 1, 1};

main ()
{
  x = (struct S) {b:0, a:0, c:({ struct S o = x; o.a == 1 ? 10 : 20;})};
  if (x.c != 10)
    abort ();
  exit (0);
}
