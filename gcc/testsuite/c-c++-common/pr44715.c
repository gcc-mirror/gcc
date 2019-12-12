/* PR c/44715 */
/* { dg-do run } */
/* { dg-options "" } */

void
foo (int x, int y)
{
  int z;
  switch (x)
    {
    case 0:
      while (({ if (y) break; 0; }))
	;
      __builtin_abort ();
      break;
    case 1:
      do
	;
      while (({ if (y) break; 0; }));
      __builtin_abort ();
      break;
    case 2:
      for (z = ({ if (y) break; 0; }); z < 5; z++)
	;
      __builtin_abort ();
      break;
    case 3:
      for (z = 0; z < ({ if (y) break; 5; }); z++)
	;
      __builtin_abort ();
      break;
    case 4:
      for (z = 0; z < 5; z += ({ if (y) break; 1; }))
	;
      __builtin_abort ();
      break;
    case 5:
      switch (({ if (y) break; 1; }))
	{
	default: break;
	}
      __builtin_abort ();
      break;
    default:
      __builtin_abort ();
      break;
    }
}

void
bar (int x, int y)
{
  int z;
  while (x >= 0)
    {
      if (x == 0)
	{
	  while (({ if (y) break; 0; }))
	    ;
	  __builtin_abort ();
	}
      if (x == 1)
	{
	  do
	    ;
	  while (({ if (y) break; 0; }));
	  __builtin_abort ();
	}
      if (x == 2)
	{
	  for (z = ({ if (y) break; 0; }); z < 5; z++)
	    ;
	  __builtin_abort ();
	}
      if (x == 3)
	{
	  for (z = 0; z < ({ if (y) break; 5; }); z++)
	    ;
	  __builtin_abort ();
	}
      if (x == 4)
	{
	  for (z = 0; z < 5; z += ({ if (y) break; 1; }))
	    ;
	  __builtin_abort ();
	}
      if (x == 5)
	{
	  switch (({ if (y) break; 1; }))
	    {
	    default: break;
	    }
	  __builtin_abort ();
	}
    }
}

void
baz (int x, int y)
{
  int z;
  while (x >= 0)
    {
      if (++y == 2)
	return;
      if (x == 0)
	{
	  while (({ if (y) continue; 0; }))
	    ;
	  __builtin_abort ();
	}
      if (x == 1)
	{
	  do
	    ;
	  while (({ if (y) continue; 0; }));
	  __builtin_abort ();
	}
      if (x == 2)
	{
	  for (z = ({ if (y) continue; 0; }); z < 5; z++)
	    ;
	  __builtin_abort ();
	}
      if (x == 3)
	{
	  for (z = 0; z < ({ if (y) continue; 5; }); z++)
	    ;
	  __builtin_abort ();
	}
      if (x == 4)
	{
	  for (z = 0; z < 5; z += ({ if (y) continue; 1; }))
	    ;
	  __builtin_abort ();
	}
      if (x == 5)
	{
	  switch (({ if (y) continue; 1; }))
	    {
	    default: break;
	    }
	  __builtin_abort ();
	}
    }
  __builtin_abort ();
}

int
main ()
{
  foo (0, 1);
  foo (1, 1);
  foo (2, 1);
  foo (3, 1);
  foo (4, 1);
  foo (5, 1);
  bar (0, 1);
  bar (1, 1);
  bar (2, 1);
  bar (3, 1);
  bar (4, 1);
  bar (5, 1);
  baz (0, 0);
  baz (1, 0);
  baz (2, 0);
  baz (3, 0);
  baz (4, 0);
  baz (5, 0);
  return 0;
}
