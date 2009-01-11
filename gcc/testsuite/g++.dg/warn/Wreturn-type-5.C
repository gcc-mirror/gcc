// PR c++/36254
// { dg-do compile }
// { dg-options "-Wreturn-type" }

int i, j, k;
struct X { X (); ~X (); };

bool
foo ()
{
  X x;
  if (i && j)
    {
      if (k)
	return true;
      else
	return false;
    }
  else
    return false;
}
