/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

struct s { __INT64_TYPE__ e; };

static void f (struct s *ps)
{
  volatile __INT64_TYPE__ m = 9223372036854775807;
  const char *str = "11E";
  int r;
  __INT64_TYPE__ sum;

  ps->e = 0;

  for (;;)
    {
      if (*str++ != '1')
	break;
      ps->e ++;
    }

  r = 1;
  sum = m;

  if (sum >= 0 && ps->e >= 0)
    {
      __UINT64_TYPE__ uc;
      uc = (__UINT64_TYPE__) sum + (__UINT64_TYPE__) ps->e;
      if (uc > 9223372036854775807)
	r = 2;
      else
	sum = 17;
    }
  else
    sum = sum + ps->e;

  if (sum != 9223372036854775807)
    __builtin_abort ();
  if (r != 2)
    __builtin_abort ();
  ps->e = sum;
}

int main (void)
{
  struct s s;
  f (&s);
  return 0;
}
