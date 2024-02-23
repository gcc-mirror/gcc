/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3" } */
#include "repeat-3.c"

int
main ()
{
  int8_t a = -17;
  int8_t b = -120;
  int8_t c = 111;
  int8_t d = -11;
  int8_t e = -34;
  int8_t f = -101;
  int8_t g = 33;
  int8_t h = 9;

  int8_t v_vnx16qi[sizeof (vnx16qi) / sizeof (int8_t)];
  f_vnx16qi (a, b, c, d, e, f, g, h, v_vnx16qi);
  for (int i = 0; i < sizeof (vnx16qi) / sizeof (int8_t); i++)
    {
      if (i % 8 == 0)
	{
	  if (v_vnx16qi[i] != a)
	    __builtin_abort ();
	}
      else if (i % 8 == 1)
	{
	  if (v_vnx16qi[i] != b)
	    __builtin_abort ();
	}
      else if (i % 8 == 2)
	{
	  if (v_vnx16qi[i] != c)
	    __builtin_abort ();
	}
      else if (i % 8 == 3)
	{
	  if (v_vnx16qi[i] != d)
	    __builtin_abort ();
	}
      else if (i % 8 == 4)
	{
	  if (v_vnx16qi[i] != e)
	    __builtin_abort ();
	}
      else if (i % 8 == 5)
	{
	  if (v_vnx16qi[i] != f)
	    __builtin_abort ();
	}
      else if (i % 8 == 6)
	{
	  if (v_vnx16qi[i] != g)
	    __builtin_abort ();
	}
      else if (i % 8 == 7)
	{
	  if (v_vnx16qi[i] != h)
	    __builtin_abort ();
	}
    }

  int8_t v_vnx32qi[sizeof (vnx32qi) / sizeof (int8_t)];
  f_vnx32qi (a, b, c, d, e, f, g, h, v_vnx32qi);
  for (int i = 0; i < sizeof (vnx32qi) / sizeof (int8_t); i++)
    {
      if (i % 8 == 0)
	{
	  if (v_vnx32qi[i] != a)
	    __builtin_abort ();
	}
      else if (i % 8 == 1)
	{
	  if (v_vnx32qi[i] != b)
	    __builtin_abort ();
	}
      else if (i % 8 == 2)
	{
	  if (v_vnx32qi[i] != c)
	    __builtin_abort ();
	}
      else if (i % 8 == 3)
	{
	  if (v_vnx32qi[i] != d)
	    __builtin_abort ();
	}
      else if (i % 8 == 4)
	{
	  if (v_vnx32qi[i] != e)
	    __builtin_abort ();
	}
      else if (i % 8 == 5)
	{
	  if (v_vnx32qi[i] != f)
	    __builtin_abort ();
	}
      else if (i % 8 == 6)
	{
	  if (v_vnx32qi[i] != g)
	    __builtin_abort ();
	}
      else if (i % 8 == 7)
	{
	  if (v_vnx32qi[i] != h)
	    __builtin_abort ();
	}
    }

  int8_t v_vnx64qi[sizeof (vnx64qi) / sizeof (int8_t)];
  f_vnx64qi (a, b, c, d, e, f, g, h, v_vnx64qi);
  for (int i = 0; i < sizeof (vnx64qi) / sizeof (int8_t); i++)
    {
      if (i % 8 == 0)
	{
	  if (v_vnx64qi[i] != a)
	    __builtin_abort ();
	}
      else if (i % 8 == 1)
	{
	  if (v_vnx64qi[i] != b)
	    __builtin_abort ();
	}
      else if (i % 8 == 2)
	{
	  if (v_vnx64qi[i] != c)
	    __builtin_abort ();
	}
      else if (i % 8 == 3)
	{
	  if (v_vnx64qi[i] != d)
	    __builtin_abort ();
	}
      else if (i % 8 == 4)
	{
	  if (v_vnx64qi[i] != e)
	    __builtin_abort ();
	}
      else if (i % 8 == 5)
	{
	  if (v_vnx64qi[i] != f)
	    __builtin_abort ();
	}
      else if (i % 8 == 6)
	{
	  if (v_vnx64qi[i] != g)
	    __builtin_abort ();
	}
      else if (i % 8 == 7)
	{
	  if (v_vnx64qi[i] != h)
	    __builtin_abort ();
	}
    }

  int8_t v_vnx128qi[sizeof (vnx128qi) / sizeof (int8_t)];
  f_vnx128qi (a, b, c, d, e, f, g, h, v_vnx128qi);
  for (int i = 0; i < sizeof (vnx128qi) / sizeof (int8_t); i++)
    {
      if (i % 8 == 0)
	{
	  if (v_vnx128qi[i] != a)
	    __builtin_abort ();
	}
      else if (i % 8 == 1)
	{
	  if (v_vnx128qi[i] != b)
	    __builtin_abort ();
	}
      else if (i % 8 == 2)
	{
	  if (v_vnx128qi[i] != c)
	    __builtin_abort ();
	}
      else if (i % 8 == 3)
	{
	  if (v_vnx128qi[i] != d)
	    __builtin_abort ();
	}
      else if (i % 8 == 4)
	{
	  if (v_vnx128qi[i] != e)
	    __builtin_abort ();
	}
      else if (i % 8 == 5)
	{
	  if (v_vnx128qi[i] != f)
	    __builtin_abort ();
	}
      else if (i % 8 == 6)
	{
	  if (v_vnx128qi[i] != g)
	    __builtin_abort ();
	}
      else if (i % 8 == 7)
	{
	  if (v_vnx128qi[i] != h)
	    __builtin_abort ();
	}
    }

  return 0;
}
