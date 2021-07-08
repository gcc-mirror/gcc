/* { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

void* malloc (size_t);
void* memcpy (void*, const void*, size_t);
size_t strlen (const char *);

// Test case reduced from gcc/attribs.c.

char* sorted_attr_string (char *argv[])
{
  size_t n = 0;
  unsigned int i;

  for (i = 0; argv[i]; ++i)
    n += strlen (argv[i]);

  char *s = (char*)malloc (n);
  n = 0;
  for (i = 0; argv[i]; ++i)
    {
      const char *str = argv[i];
      size_t len = strlen (str);
      memcpy (s + n, str, len);
      n += len + 1;
    }

  /* Replace "=,-" with "_".  */
  for (i = 0; i < strlen (s); i++)
    if (s[i] == '=')
      s[i] = '_';             // { dg-bogus "\\\[-Wstringop-overflow" }

  return s;
}


void f (void*);

void nowarn_cond_escape (int c, int *x)
{
  extern char a3[3], a5[5];

  char *p;
  if (c)
    {
      p = a3;
      *x = 2;
   }
   else
     {
       p = a5;
       *x = 4;
     }

  f (p);   // may modify *x

  if (*x == 2)
    p[2] = 0;
  else if (*x == 4)
    p[4] = 0;                 // { dg-bogus "\\\[-Wstringop-overflow" }
}

void warn_cond_escape (int c, int *x)
{
  extern char a3_2[3];
  extern char a5_2[5];        // { dg-message "at offset 5 into object 'a5_2'" }

  char *p;
  if (c)
    {
      p = a3_2;
      *x = 2;
   }
   else
     {
       p = a5_2;
       *x = 5;
     }

  f (p);   // may modify *x

  if (*x == 2)
    p[2] = 0;
  else if (*x == 5)
    p[5] = 0;                 // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
}
