/* { dg-do compile } */
/* { dg-options "-O2" } */

__extension__ typedef __SIZE_TYPE__ size_t;

int
php_filter_parse_int (char const *str, unsigned int str_len, long *ret)
{
  long ctx_value;
  int sign;
  int digit;
  char const *end;
  int tmp;
  char const *tmp___0;
  char const *tmp___1;

  sign = 0;
  digit = 0;
  end = str + str_len;
  switch ((int) *str)
    {
    case 45:
      sign = 1;
    case 43:
      str++;
    default:;
      break;
    }
  if ((size_t) str < (size_t) end)
    {
      if ((int const) *str >= 49)
	{
	  if ((int const) *str <= 57)
	    {
	      if (sign)
		{
		  tmp = -1;
		}
	      else
		{
		  tmp = 1;
		}
	      tmp___0 = str;
	      str++;
	      ctx_value = (long) (tmp * (int) ((int const) *tmp___0 - 48));
	    }
	  else
	    {
	      return (-1);
	    }
	}
      else
	{
	  return (-1);
	}
    }
  else
    {
      return (-1);
    }
  if (end - str > 19)
    {
      return (-1);
    }
  while ((size_t) str < (size_t) end)
    {
      if ((int const) *str >= 48)
	{
	  if ((int const) *str <= 57)
	    {
	      tmp___1 = str;
	      str++;
	      digit = (int) ((int const) *tmp___1 - 48);
	      if (!sign)
		{
		  if (ctx_value <=
		      (9223372036854775807L - (long) digit) / 10L)
		    {
		      ctx_value = ctx_value * 10L + (long) digit;
		    }
		  else
		    {
		      goto _L;
		    }
		}
	      else
		{
		_L:
		  if (sign)
		    {
		      if (ctx_value >=
			  ((-0x7FFFFFFFFFFFFFFF - 1) + (long) digit) / 10L)
			{
			  ctx_value = ctx_value * 10L - (long) digit;
			}
		      else
			{
			  return (-1);
			}
		    }
		  else
		    {
		      return (-1);
		    }
		}
	    }
	  else
	    {
	      return (-1);
	    }
	}
      else
	{
	  return (-1);
	}
    }
  *ret = ctx_value;
  return (1);
}

/* { dg-final { scan-assembler-not "idiv" } } */
