// PR opt/13869
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort ();

int test ()
{
  bool my_bool = true;
  for (int i = 0; i < 10; ++i)
    {
      if (!my_bool)
	;
      else
	my_bool = false;
    };
  return my_bool;
}

int main ()
{
  if (test ())
    abort ();
  return 0;
}
