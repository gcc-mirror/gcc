/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3" } */

#include "repeat-5.c"

int
main ()
{
  int8_t a = -122;

  int8_t v_vnx2qi[sizeof (vnx2qi) / sizeof (int8_t)];
  f_vnx2qi (a, v_vnx2qi);
  for (int i = 0; i < sizeof (vnx2qi) / sizeof (int8_t); i++)
    {
      if (i % 2 == 0)
	{
	  if (v_vnx2qi[i] != a)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx2qi[i] != -88)
	    __builtin_abort ();
	}
    }

  int8_t v_vnx4qi[sizeof (vnx4qi) / sizeof (int8_t)];
  f_vnx4qi (a, v_vnx4qi);
  for (int i = 0; i < sizeof (vnx4qi) / sizeof (int8_t); i++)
    {
      if (i % 2 == 0)
	{
	  if (v_vnx4qi[i] != a)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx4qi[i] != -88)
	    __builtin_abort ();
	}
    }

  int8_t v_vnx8qi[sizeof (vnx8qi) / sizeof (int8_t)];
  f_vnx8qi (a, v_vnx8qi);
  for (int i = 0; i < sizeof (vnx8qi) / sizeof (int8_t); i++)
    {
      if (i % 2 == 0)
	{
	  if (v_vnx8qi[i] != a)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx8qi[i] != -88)
	    __builtin_abort ();
	}
    }

  int8_t v_vnx16qi[sizeof (vnx16qi) / sizeof (int8_t)];
  f_vnx16qi (a, v_vnx16qi);
  for (int i = 0; i < sizeof (vnx16qi) / sizeof (int8_t); i++)
    {
      if (i % 2 == 0)
	{
	  if (v_vnx16qi[i] != a)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx16qi[i] != -88)
	    __builtin_abort ();
	}
    }

  int8_t v_vnx32qi[sizeof (vnx32qi) / sizeof (int8_t)];
  f_vnx32qi (a, v_vnx32qi);
  for (int i = 0; i < sizeof (vnx32qi) / sizeof (int8_t); i++)
    {
      if (i % 2 == 0)
	{
	  if (v_vnx32qi[i] != a)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx32qi[i] != -88)
	    __builtin_abort ();
	}
    }

  int8_t v_vnx64qi[sizeof (vnx64qi) / sizeof (int8_t)];
  f_vnx64qi (a, v_vnx64qi);
  for (int i = 0; i < sizeof (vnx64qi) / sizeof (int8_t); i++)
    {
      if (i % 2 == 0)
	{
	  if (v_vnx64qi[i] != a)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx64qi[i] != -88)
	    __builtin_abort ();
	}
    }

  int8_t v_vnx128qi[sizeof (vnx128qi) / sizeof (int8_t)];
  f_vnx128qi (a, v_vnx128qi);
  for (int i = 0; i < sizeof (vnx128qi) / sizeof (int8_t); i++)
    {
      if (i % 2 == 0)
	{
	  if (v_vnx128qi[i] != a)
	    __builtin_abort ();
	}
      else
	{
	  if (v_vnx128qi[i] != -88)
	    __builtin_abort ();
	}
    }

  return 0;
}
