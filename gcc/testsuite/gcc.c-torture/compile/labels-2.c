struct bp { void *v, *b, *e; };
f ()
{
  struct bp x = { &&L2 };
  if (&&L3 - &&L1 > 1)
    abort ();
L1:return 1;
L2:abort ();
L3:;
}
