/* PR middle-end/81117 - Improve buffer overflow checking in strncpy
   { dg-do compile }
   { dg-options "-O2 -Wstringop-overflow -Wno-stringop-truncation -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

#if __cplusplus
extern "C" {
#endif

size_t strlen (const char*);
char* strncat (char*, const char*, size_t);
char* strncpy (char*, const char*, size_t);
#if __cplusplus
}
#endif

const char ar[] = "123";

void test_strncat (char **d, const char* s, int i)
{
  /* Use a fresh pointer for each test to prevent the optimizer from
     eliminating redundant writes into the same destination.  Avoid
     calling functions like sink() on the result that would have to
     be assumed to change the source string by the alias oracle.  */
#define T(d, s, len) strncat (*d++, (s), (len))

  T (d, "",    0);
  T (d, "",    1);
  T (d, "",    2);
  T (d, "",    3);
  T (d, "123", 0);
  /* The following two calls truncate the copy and are diagnosed
     by -Wstringop-truncation but there is evidence of overflow so
     they're not diagnosed by -Wstringop-overflow.  */
  T (d, "123", 1);
  T (d, "123", 2);

  T (d, "123", 3);                /* { dg-warning ".strncat\[^\n\r\]* specified bound 3 equals source length" } */
  T (d, "123", 4);
  T (d, "123", 9);

  T (d, s, strlen (s));           /* { dg-warning ".strncat\[^\n\r\]* specified bound depends on the length of the source argument" } */
  T (d, s, strlen (s) + 1);       /* { dg-warning ".strncat\[^\n\r\]* specified bound depends on the length of the source argument" } */
  /* The following could also be diagnosed by -Wstringop-truncation
     (with some effort to distinguish the pattern from others like
     the one above.  */
  T (d, s, strlen (s) - 1);       /* { dg-warning ".strncat\[^\n\r\]* specified bound depends on the length of the source argument" } */
  T (d, s, strlen (s) - i);       /* { dg-warning ".strncat\[^\n\r\]* specified bound depends on the length of the source argument" } */

  /* The following is dubious but not necessarily a smoking gun.  */
  T (d, s, strlen (s) - strlen (s));

  {
    signed char n = strlen (s);   /* { dg-message "length computed here" } */
    T (d, s, n);                  /* { dg-warning ".strncat\[^\n\r\]* specified bound depends on the length of the source argument" } */
  }

  {
    short n = strlen (s);         /* { dg-message "length computed here" } */
    T (d, s, n);                  /* { dg-warning ".strncat\[^\n\r\]* specified bound depends on the length of the source argument" } */
  }

  {
    int n = strlen (s);           /* { dg-message "length computed here" } */
    T (d, s, n);                  /* { dg-warning ".strncat\[^\n\r\]* specified bound depends on the length of the source argument" } */
  }

  {
    unsigned n = strlen (s);      /* { dg-message "length computed here" } */
    T (d, s, n);                  /* { dg-warning ".strncat\[^\n\r\]* specified bound depends on the length of the source argument" } */
  }

  {
    size_t n;
    n = strlen (s);               /* { dg-message "length computed here" } */
    T (d, s, n);                  /* { dg-warning ".strncat\[^\n\r\]* specified bound depends on the length of the source argument" } */
  }

  {
    size_t n;
    n = strlen (s) - 1;           /* { dg-message "length computed here" } */
    T (d, s, n);                  /* { dg-message ".strncat\[^\n\r\]* specified bound depends on the length of the source argument" } */
  }

  {
    /* This doesn't overflow so iit should not be diagnosed.  */
    size_t n = strlen (s) - strlen (s);
    T (d, s, n);
  }

  {
    size_t n = i < strlen (s) ? i : strlen (s);   /* { dg-message "length computed here" } */
    T (d, s, n);                  /* { dg-message ".strncat\[^\n\r\]* specified bound depends on the length of the source argument" } */
  }
}


void test_strncpy (char **d, const char* s, int i)
{
#undef T
#define T(d, s, len) strncpy (*d++, (s), (len))

  T (d, "",    0);
  T (d, "",    1);
  T (d, "",    2);
  T (d, "",    3);
  T (d, "123", 0);
  T (d, "123", 1);
  T (d, "123", 2);
  T (d, "123", 3);
  T (d, "123", 4);
  T (d, "123", 9);

  T (d, "123", sizeof "123");
  T (d, ar, sizeof ar);

  /* There is no overflow in the following calls but they are diagnosed
     by -Wstringop-truncation.  Verify that they aren'y also diagnosed
     by -Wstringop-overflow.  */
  T (d, s, strlen (s));

  {
    int n = strlen (s);
    T (d, s, n);
  }

  {
    unsigned n = strlen (s);
    T (d, s, n);
  }

  {
    size_t n;
    n = strlen (s);
    T (d, s, n);
  }

  {
    size_t n;
    n = strlen (s) - 1;
    T (d, s, n);
  }

  {
    /* This is diagnosed by -Wstringop-truncation.  Verify that it isn't
       also diagnosed by -Wstringop-overflow.  */
    size_t n = strlen (s) - strlen (s);
    T (d, s, n);
  }

  {
    /* This use of strncpy is certainly dubious and it could well be
       diagnosed by -Wstringop-truncation but it isn't.  */
    size_t n = i < strlen (s) ? i : strlen (s);   /* { dg-message "length computed here" "note" { xfail *-*-* } } */
    T (d, s, n);                  /* { dg-message ".strncpy\[^\n\r]* specified bound depends on the length of the source argument" "pr?????" { xfail *-*-* } } */
  }
}
