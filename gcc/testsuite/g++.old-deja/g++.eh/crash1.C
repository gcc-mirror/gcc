// Build don't link:
// Special g++ Options: -O1 -fno-inline-functions

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
