/* PR tree-optimization/84468 - bogus -Wstringop-truncation despite
   assignment after conditional strncpy
   { dg-do compile }
   { dg-options "-O2 -Wstringop-truncation -g" } */

extern char* strncpy (char*, const char*, __SIZE_TYPE__);

char a[4];

void f1 (char *s)
{
  int i = 0;

  if (s[0] == '0')
    {
      i += 1;
      strncpy (a, s, sizeof a);                   /* { dg-bogus "\\\[-Wstringop-truncation]" } */
    }
  else
    i += 2;

  a[sizeof a - 1] = 0;
}

void f2 (char *s)
{
  int i = 0;

  if (s[0] == '0')
    {
      i += 1;
      if (s[1] == '1')
	{
	  i += 2;
	  strncpy (a, s, sizeof a);               /* { dg-bogus "\\\[-Wstringop-truncation]" } */
	}
      else
	i += 3;
    }
  else
    i += 4;

  a[sizeof a - 1] = 0;
}

void f3 (char *s)
{
  int i = 0;

  if (s[0] == '0')
    {
      i += 1;
      if (s[1] == '1')
	{
	  i += 2;
	  if (s[2] == '2')
	    strncpy (a, s, sizeof a);             /* { dg-bogus "\\\[-Wstringop-truncation]" } */
	  else
	    i += 3;
	}
      else
	i += 4;
    }
  else
    i += 5;

  a[sizeof a - 1] = 0;
}

void f4 (char *s)
{
  int i = 0;

  if (s[0] == '0')
    {
      i += 1;
      if (s[1] == '1')
	{
	  i += 2;
	  if (s[2] == '2')
	    {
	      i += 3;
	      if (s[3] == '3')
		strncpy (a, s, sizeof a);         /* { dg-bogus "\\\[-Wstringop-truncation]" } */
	      else
		i += 4;
	    }
	  else
	    i += 5;
	}
      else
	i += 6;
    }
  else
    i += 7;

  a[sizeof a - 1] = 0;
}

void f4_warn (char *s)
{
  int i = 0;

  if (s[0] == '0')
    {
      i += 1;
      if (s[1] == '1')
	{
	  i += 2;
	  if (s[2] == '2')
	    {
	      i += 3;
	      if (s[3] == '3')
		strncpy (a, s, sizeof a);         /* { dg-warning "\\\[-Wstringop-truncation]" } */
	      else
		i += 4;
	    }
	  else
	    i += 5;
	}
      else
	i += 6;
    }
  else
    i += 7;
}
