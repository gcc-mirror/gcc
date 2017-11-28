// { dg-do assemble  }
// { dg-options "-O1 -fno-inline-functions -Wno-return-type" }

struct A
{
  ~A ();
};

bool foo ();

int i;
int j;

A bar ()
{
  for (i = 0; i < 1; ++i)
    if (j)
      {
	A tmp;
	return tmp;
      }
}
