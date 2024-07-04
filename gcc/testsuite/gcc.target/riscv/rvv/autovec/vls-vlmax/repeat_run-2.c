/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3" } */

#include "repeat-2.c"

int
main ()
{
  int8_t a = -17;
  int8_t b = -120;
  int8_t c = 111;
  int8_t d = -11;

  int8_t v_vnx8qi[sizeof (vnx8qi) / sizeof (int8_t)];
  f_vnx8qi (a, b, c, d, v_vnx8qi);
  for (int i = 0; i < sizeof (vnx8qi) / sizeof (int8_t); i++)
    {
      if (i % 4 == 0)
	{
	  if (v_vnx8qi[i] != a)
	    __builtin_abort ();
	}
      else if (i % 4 == 1)
	{
	  if (v_vnx8qi[i] != b)
	    __builtin_abort ();
	}
      else if (i % 4 == 2)
	{
	  if (v_vnx8qi[i] != c)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx8qi[i] != d)
	    __builtin_abort ();
	}
    }

  int8_t v_vnx16qi[sizeof (vnx16qi) / sizeof (int8_t)];
  f_vnx16qi (a, b, c, d, v_vnx16qi);
  for (int i = 0; i < sizeof (vnx16qi) / sizeof (int8_t); i++)
    {
      if (i % 4 == 0)
	{
	  if (v_vnx16qi[i] != a)
	    __builtin_abort ();
	}
      else if (i % 4 == 1)
	{
	  if (v_vnx16qi[i] != b)
	    __builtin_abort ();
	}
      else if (i % 4 == 2)
	{
	  if (v_vnx16qi[i] != c)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx16qi[i] != d)
	    __builtin_abort ();
	}
    }

  int8_t v_vnx32qi[sizeof (vnx32qi) / sizeof (int8_t)];
  f_vnx32qi (a, b, c, d, v_vnx32qi);
  for (int i = 0; i < sizeof (vnx32qi) / sizeof (int8_t); i++)
    {
      if (i % 4 == 0)
	{
	  if (v_vnx32qi[i] != a)
	    __builtin_abort ();
	}
      else if (i % 4 == 1)
	{
	  if (v_vnx32qi[i] != b)
	    __builtin_abort ();
	}
      else if (i % 4 == 2)
	{
	  if (v_vnx32qi[i] != c)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx32qi[i] != d)
	    __builtin_abort ();
	}
    }

  int8_t v_vnx64qi[sizeof (vnx64qi) / sizeof (int8_t)];
  f_vnx64qi (a, b, c, d, v_vnx64qi);
  for (int i = 0; i < sizeof (vnx64qi) / sizeof (int8_t); i++)
    {
      if (i % 4 == 0)
	{
	  if (v_vnx64qi[i] != a)
	    __builtin_abort ();
	}
      else if (i % 4 == 1)
	{
	  if (v_vnx64qi[i] != b)
	    __builtin_abort ();
	}
      else if (i % 4 == 2)
	{
	  if (v_vnx64qi[i] != c)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx64qi[i] != d)
	    __builtin_abort ();
	}
    }

  int8_t v_vnx128qi[sizeof (vnx128qi) / sizeof (int8_t)];
  f_vnx128qi (a, b, c, d, v_vnx128qi);
  for (int i = 0; i < sizeof (vnx128qi) / sizeof (int8_t); i++)
    {
      if (i % 4 == 0)
	{
	  if (v_vnx128qi[i] != a)
	    __builtin_abort ();
	}
      else if (i % 4 == 1)
	{
	  if (v_vnx128qi[i] != b)
	    __builtin_abort ();
	}
      else if (i % 4 == 2)
	{
	  if (v_vnx128qi[i] != c)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx128qi[i] != d)
	    __builtin_abort ();
	}
    }

  return 0;
}
