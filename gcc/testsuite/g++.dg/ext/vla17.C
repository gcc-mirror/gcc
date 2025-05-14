// PR c++/69487
// { dg-do run }
// { dg-options -Wno-vla }

int size = 6;

int main()
{
  char buffer[size] = "";
  for (int i = 0; i != size; ++i)
    {
      if (buffer[i] != 0)
	__builtin_abort();
    }
}
