// PR c++/80642
// { dg-do compile { target c++14 } }

int main()
{
  [](auto i)
    {
      if (i)
        {
	  int j;
	  static int k;
	  return i + j;
        }
      return i;
    }(0);
}
