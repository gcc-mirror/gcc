// P0614R1
// { dg-do compile }
// { dg-options "-std=c++2a" }

void
fn1 ()
{
  int a[] = { 1, 2, 3, 4, 5 };

  for (int i = 0; auto x : a)
    ++i;

  i = 0; // { dg-error "not declared" }

  for (int i = 0; auto x : a)
    {
      for (int j = 0; auto x : a)
	{
	  for (int k = 0; auto x : a)
	    k++;
	  k++; // { dg-error "not declared" }
	}
      j++; // { dg-error "not declared" }
    }
}

void
fn2 ()
{
  int a[] = { 1, 2, 3, 4, 5 };
  for (int i = 0; auto x : a)
    int i = 3; // { dg-error "redeclaration" }
}
void
fn3 ()
{
  int a[] = { 1, 2, 3, 4, 5 };

  for (;:) // { dg-error "expected" }
    {
    }

  for (;;:) // { dg-error "expected" }
    {
    }
}
