/* { dg-do compile } */
/* { dg-options "-O1" } */

/* A test for unreachable blocks removal -- bind_expr whose entry is
   unreachable, but it contains reachable statements.  */   

void foo(void)
{
  if (1)
    {
      goto bla;
    }
  else
    {
xxx:
	{
bla:
	  bar ();
	  return;
	}
      goto xxx;
    }
}

