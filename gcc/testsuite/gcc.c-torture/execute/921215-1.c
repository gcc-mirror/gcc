main()
{
#ifndef NO_TRAMPOLINES
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
#endif
  exit(0);
}

foo(){}
