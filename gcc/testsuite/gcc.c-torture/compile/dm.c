struct dm
{
  unsigned q;
  unsigned r;
};

struct dm
dm (a, b)
     unsigned a, b;
{
  struct dm qr;

  qr.q = a / b;
  qr.r = a % b;
  return qr;
}

main ()
{
  struct dm qr;

  qr = dm (100, 30);
  printf ("%u, %u\n", qr.q, qr.r);
}
