/* { dg-require-effective-target trampolines } */
/* { dg-additional-options "-std=gnu89" } */

main()
{
  void p(void ((*f) (void ())))
    {
      void r()
	{
	  foo ();
	}

      f(r);
    }

  void q(void ((*f)()))
    {
      f();
    }

  p(q);

  exit(0);
}

foo(){}
