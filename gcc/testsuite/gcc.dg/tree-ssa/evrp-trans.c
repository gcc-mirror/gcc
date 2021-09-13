/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

/* Simple tests to make sure transitives are working. */
void keep();
void kill();

void
f1 (int x, int y, int z)
{
  if (x > y)
    if (y > z)
      {
	if (x > z)
	  keep ();
	else
	  kill ();
      }
}

void
f2 (int w, int x, int y, int z)
{  
  // Test one equivalence.
  if (w == z)
    if (x > y)
      if (y > z)
	{
	  if (x > w)
	    keep ();
	  else
	    kill ();
	}
}

void
f3 (int a, int w, int x, int y, int z)
{  
  // Test two equivlaences.
  if (a == x)
    if (w == z)
      if (x > y)
	if (y > z)
	  {
	    if (a > w)
	      keep ();
	    else
	      kill ();
	  }
}

void
f4 (int x, int y, int z)
{
  // test X > Y >= Z
  if (x > y)
    if (y >= z)
      {
        if (x > z)
          keep ();
        else
          kill ();
      }
}
void
f5 (int x, int y, int z)
{
  // test X >= Y > Z
  if (x >= y)
    if (y > z)
      {
        if (x > z)
          keep ();
        else
          kill ();
      }
}

void
f6 (int x, int y, int z)
{
  // test X >= Y >= Z
  if (x >= y)
    if (y >= z)
      {
        if (x > z)
          keep ();
        else if (x == z)
	  keep ();
         else
          kill ();
      }
}

void
f7 (int x, int y, int z)
{
  // test Y <= X , Z <= Y
  if (y <= x)
    if (z <= y)
      {
        if (x > z)
          keep ();
        else if (x == z)
	  keep ();
	else
          kill ();
      }
}

void
f8 (int x, int y, int z)
{
  // test X >= Y, Z <= Y
  if (x >= y)
    if (z <= y)
      {
        if (x > z)
          keep ();
        else if (x == z)
	  keep ();
	else
          kill ();
      }
}

void
f9 (int x, int y, int z)
{
  // test Y <= X   Y >= Z
  if (y <= x)
    if (y >= z)
      {
        if (x > z)
          keep ();
        else if (x == z)
	  keep ();
        else
          kill ();
      }
}

/* { dg-final { scan-tree-dump-not "kill" "evrp" } }  */
/* { dg-final { scan-tree-dump-times "keep" 13 "evrp"} } */
