/* PR middle-end/95673 - missing -Wstring-compare for an impossible strncmp test
   { dg-do compile }
   { dg-options "-O2 -Wall -Wstring-compare -Wno-stringop-overread -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

extern int strcmp (const char*, const char*);
extern int strncmp (const char*, const char*, size_t);

void sink (int, ...);

extern char a3[3];

int nowarn_strcmp_one_use_ltz (int c)
{
  const char *s = c ? "1234" : a3;
  int n = strcmp (s, "123");
  return n < 0;
}


int nowarn_strcmp_one_use_eqnz (int c)
{
  const char *s = c ? "12345" : a3;
  int n = strcmp (s, "123");
  return n == 1;
}


int warn_strcmp_one_use_eqz (int c)
{
  const char *s = c ? "123456" : a3;
  int n = strcmp (s, "123");    // { dg-warning "'strcmp' of a string of length 3 and an array of size 3 evaluates to nonzero" }
  return n == 0;                // { dg-message "in this expression" }
}


int warn_strcmp_one_use_bang (int c)
{
  const char *s = c ? "1234567" : a3;
  int n = strcmp (s, "123");    // { dg-warning "'strcmp' of a string of length 3 and an array of size 3 evaluates to nonzero" }
  return !n;                    // { dg-message "in this expression" }
}


int warn_strcmp_one_use_bang_bang (int c)
{
  const char *s = c ? "12345678" : a3;
  int n = strcmp (s, "123");    // { dg-warning "'strcmp' of a string of length 3 and an array of size 3 evaluates to nonzero" }
  return !!n;                   // { dg-message "in this expression" }
}


_Bool warn_one_use_bool (int c)
{
  const char *s = c ? "123456789" : a3;
  int n = strcmp (s, "123");    // { dg-warning "'strcmp' of a string of length 3 and an array of size 3 evaluates to nonzero" }
  return (_Bool)n;              // { dg-message "in this expression" }
}


int warn_strcmp_one_use_cond (int c)
{
  const char *s = c ? "1234567890" : a3;
  int n = strcmp (s, "123");    // { dg-warning "'strcmp' of a string of length 3 and an array of size 3 evaluates to nonzero" }
  return n ? 3 : 5;             // { dg-message "in this expression" }
}


int nowarn_strcmp_multiple_uses (int c)
{
  const char *s = c ? "1234" : a3;
  int n = strcmp (s, "123");
  sink (n < 0);
  sink (n > 0);
  sink (n <= 0);
  sink (n >= 0);
  sink (n + 1);
  return n;
}


int warn_strcmp_multiple_uses (int c)
{
  const char *s = c ? "12345" : a3;
  int n = strcmp (s, "123");    // { dg-warning "'strcmp' of a string of length 3 and an array of size 3 evaluates to nonzero" }
  sink (n < 0);
  sink (n > 0);
  sink (n <= 0);
  sink (n >= 0);
  sink (n == 0);                // { dg-message "in this expression" }
  return n;
}


int warn_strncmp_multiple_uses (int c)
{
  const char *s = a3;
  int n = strncmp (s, "1234", 4); // { dg-warning "'strncmp' of a string of length 4, an array of size 3 and bound of 4 evaluates to nonzero" }
  sink (n < 0);
  sink (n > 0);
  sink (n <= 0);
  sink (n >= 0);
  sink (n == 0);                // { dg-message "in this expression" }
  return n;
}
