typedef struct {
     double epsilon;
} material_type;

material_type foo(double x)
{
     material_type m;

     m.epsilon = 1.0 + x;
     return m;
}

main()
{
  int i;
  material_type x;

  /* We must iterate enough times to overflow the FP stack on the
     x86.  */
  for (i = 0; i < 10; i++)
    {
      x = foo (1.0);
      if (x.epsilon != 1.0 + 1.0)
	abort ();
    }

  exit (0);
}
