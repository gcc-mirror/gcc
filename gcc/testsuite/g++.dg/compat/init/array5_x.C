extern "C" void abort (void);

extern int count;
extern int num;

struct A
{
  A();
  ~A();
};

struct Array
{
  A array[2][2][2];
};

void
array5_x ()
{
  for (num = 0; num <= 8; ++num)
    {
      count = 0;
      try
	{
	  Array A;
	}
      catch (...)
        {
	}
      if (count != 0)
	abort();
    }
}
