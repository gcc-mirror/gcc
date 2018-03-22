/* PR/tree-optimization/84480 - bogus -Wstringop-truncation despite
   assignment with an inlined string literal
   { dg-do compile }
   { dg-options "-O2 -Wstringop-truncation" }  */

#include <string.h>

template <size_t N>
class GoodString
{
public:
  GoodString (const char *s, size_t slen = N)
  {
    if (slen > N)
      slen = N;

    strncpy (str, s, slen);

    str[slen] = '\0';
  }

private:
  char str[N + 1];
};

void sink (void*);

void good_nowarn_size_m2 ()
{
  GoodString<3> str ("12");
  sink (&str);
}

void good_nowarn_size_m1 ()
{
  GoodString<3> str ("123");    // { dg-bogus "\\\[-Wstringop-truncation]" }
  sink (&str);
}

static void good_nowarn_size_m1_var (const char* s)
{
  GoodString<3> str (s);        // { dg-bogus "\\\[-Wstringop-truncation]" }
  sink (&str);
}

void call_good_nowarn_size_m1_var ()
{
  good_nowarn_size_m1_var ("456");
}


template <size_t N>
class BadString1
{
public:
  BadString1 (const char *s, size_t slen = N)
  {
    if (slen > N)
      slen = N;

    strncpy (str, s, slen);
  }

private:
  char str[N + 1];
};

void bad1_nowarn_size_m2 ()
{
  BadString1<3> str ("12");
  sink (&str);
}


template <size_t N>
class BadString2
{
public:
  BadString2 (const char *s, size_t slen = N)
  {
    if (slen > N)
      slen = N;

    strncpy (str, s, slen);     // { dg-warning "\\\[-Wstringop-truncation]" }
  }

private:
  char str[N + 1];
};

void bad2_warn_size_m1 ()
{
  BadString2<3> str ("123");
  sink (&str);
}

// { dg-message "inlined from .void bad2_warn_size_m1." "" { target *-*-* } 0 }

template <size_t N>
class BadString3
{
public:
  BadString3 (const char *s, size_t slen = N)
  {
    if (slen > N)
      slen = N;

    strncpy (str, s, slen);     // { dg-warning "\\\[-Wstringop-truncation]" }
  }

private:
  char str[N + 1];
};

static void bad3_warn_size_m1_var (const char *s)
{
  BadString3<3> str (s);
  sink (&str);
}

void call_bad3_warn_size_m1_var ()
{
  bad3_warn_size_m1_var ("456");
}

// { dg-message "inlined from .void call_bad3_warn_size_m1_var." "" { target *-*-* } 0 }
