/* Verify that an anti-range ~[A, B] with small positive A and B
   is handled correctly and doesn't trigger warnings.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __typeof__ (sizeof 0) size_t;

int f (void*, size_t);
int g (void*);

// Test case distilled from gcc/cp/semantics.c

int omp_reduction_id (int i, int j, const char *mm)
{
  const char *p = 0;
  const char *m = 0;

  switch (i)
    {
    case 1:
      p = "min";
      break;
    case 2:
      p = "max";
      break;
    default:
      break;
    }

  if (j)
    m = mm;

  const char prefix[] = "omp declare reduction ";
  size_t lenp = sizeof (prefix);

  if (__builtin_strncmp (p, prefix, lenp - 1) == 0)
    lenp = 1;

  size_t len = __builtin_strlen (p);
  size_t lenm = m ? __builtin_strlen (m) + 1 : 0;
  char *name = ((char *) __builtin_alloca(lenp + len + lenm));

  if (lenp > 1)
    __builtin_memcpy (name, prefix, lenp - 1);

  __builtin_memcpy (name + lenp - 1, p, len + 1);
  if (m)
    {
      name[lenp + len - 1] = '~';
      __builtin_memcpy (name + lenp + len, m, lenm);
    }
  return (__builtin_constant_p (name)
	  ? f (name, __builtin_strlen (name)) : g (name));
}

// Test case derived from gcc/d/dmd/root/filename.c.

const char *ext (const char *str)
{
  size_t len = __builtin_strlen(str);

  const char *e = str + len;
  for (;;)
    {
      switch (*e)
        {
	case '.': return e + 1;
	case '/': break;
	default:
	  if (e == str)
	    break;
	  e--;
	  continue;
        }
      return 0;
    }
}

const char *removeExt (const char *str)
{
  const char *e = ext (str);
  if (e)
    {
      size_t len = (e - str) - 1;
      char *n = (char *)__builtin_malloc (len + 1);
      __builtin_memcpy(n, str, len);
      n[len] = 0;
      return n;
    }
  return 0;
}
