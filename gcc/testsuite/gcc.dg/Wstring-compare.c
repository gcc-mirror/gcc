/* PR tree-optimization/90879 - fold zero-equality of strcmp between
   a longer string and a smaller array
   { dg-do compile }
   { dg-options "-O2 -Wall -Wextra -ftrack-macro-expansion=0" } */

#include "strlenopt.h"

#define T(a, b) sink (0 == strcmp (a, b), a, b)

void sink (int, ...);

struct S { char a4[4], c; };

extern char a4[4];
extern char a5[5];
extern char b4[4];

/* Verify that comparison of string literals with arrays with unknown
   content but size that prevents them from comparing equal is diagnosed.  */

void strcmp_array_lit (void)
{
  if (strcmp (a4, "1234"))  // { dg-warning "'strcmp' of a string of length 4 and an array of size 4 evaluates to nonzero" }
                            // { dg-bogus "in this expreession" "unwanted note" { target *-*-* } .-1 }
    sink (0, a4);

  int cmp;
  cmp = strcmp (a4, "1234");  // { dg-warning "'strcmp' of a string of length 4 and an array of size 4 evaluates to nonzero" }
  if (cmp)                  // { dg-message "in this expression" }
    sink (0, a4);

  T (a4, "4321");           // { dg-warning "'strcmp' of a string of length 4 and an array of size 4 evaluates to nonzero " }
  T (a4, "12345");          // { dg-warning "length 5 and an array of size 4 " }
  T (a4, "123456");         // { dg-warning "length 6 and an array of size 4 " }
  T ("1234", a4);           // { dg-warning "length 4 and an array of size 4 " }
  T ("12345", a4);          // { dg-warning "length 5 and an array of size 4 " }
  T ("123456", a4);         // { dg-warning "length 6 and an array of size 4 " }
}


void strcmp_array_pstr (void)
{
  const char *s4 = "1234";

  {
    if (strcmp (a4, s4))    // { dg-warning "'strcmp' of a string of length 4 and an array of size 4 evaluates to nonzero" }
                            // { dg-bogus "in this expreession" "unwanted note" { target *-*-* } .-1 }
      sink (1, a4);
    else
      sink (0, a4);
  }

  {
    int c;
    c = strcmp (a4, s4);    // { dg-warning "'strcmp' of a string of length 4 and an array of size 4 evaluates to nonzero" }
    if (c)                  // { dg-message "in this expression" }
      sink (1, a4);
    else
      sink (0, a4);
  }

  const char *t4 = "4321";
  const char *s5 = "12345";
  const char *s6 = "123456";

  T (a4, t4);               // { dg-warning "'strcmp' of a string of length 4 and an array of size 4 evaluates to nonzero " }
  T (a4, s5);               // { dg-warning "length 5 and an array of size 4 " }
  T (a4, s6);               // { dg-warning "length 6 and an array of size 4 " }
  T (s4, a4);               // { dg-warning "length 4 and an array of size 4 " }
  T (s5, a4);               // { dg-warning "length 5 and an array of size 4 " }
  T (s6, a4);               // { dg-warning "length 6 and an array of size 4 " }
}


void strcmp_array_cond_pstr (int i)
{
  const char *s4 = i ? "1234" : "4321";
  T (a4, s4);               // { dg-warning "'strcmp' of a string of length 4 and an array of size 4 evaluates to nonzero " }
  T (a5, s4);
}

void strcmp_array_copy (void)
{
  char s[8];

  {
    strcpy (s, "1234");
    if (strcmp (a4, s))     // { dg-warning "'strcmp' of a string of length 4 and an array of size 4 evaluates to nonzero" }
                            // { dg-bogus "in this expreession" "unwanted note" { target *-*-* } .-1 }
      sink (1, a4);
    else
      sink (0, a4);
  }

  {
    strcpy (s, "1234");

    int c;
    c = strcmp (a4, s);     // { dg-warning "'strcmp' of a string of length 4 and an array of size 4 evaluates to nonzero" }
    if (c)                  // { dg-message "in this expression" }
      sink (1, a4);
    else
      sink (0, a4);
  }

  strcpy (s, "4321");
  T (a4, s);                // { dg-warning "'strcmp' of a string of length 4 and an array of size 4 evaluates to nonzero " }
  strcpy (s, "12345");
  T (a4, s);                // { dg-warning "length 5 and an array of size 4 " }
  strcpy (s, "123456");
  T (a4, s);                // { dg-warning "length 6 and an array of size 4 " }
  strcpy (s, "4321");
  T (s, a4);                // { dg-warning "length 4 and an array of size 4 " }
  strcpy (s, "54321");
  T (s, a4);                // { dg-warning "length 5 and an array of size 4 " }
  strcpy (s, "654321");
  T (s, a4);                // { dg-warning "length 6 and an array of size 4 " }
}


void strcmp_member_array_lit (const struct S *p)
{
  T (p->a4, "1234");        // { dg-warning "length 4 and an array of size 4 " }
}


#undef T
#define T(a, b, n) sink (0 == strncmp (a, b, n), a, b)

void strncmp_array_lit (void)
{
  if (strncmp (a4, "12345", 5))   // { dg-warning "'strncmp' of a string of length 5, an array of size 4 and bound of 5 evaluates to nonzero" }
                                  // { dg-bogus "in this expreession" "unwanted note" { target *-*-* } .-1 }
    sink (0, a4);

  int cmp;
  cmp = strncmp (a4, "54321", 5);   // { dg-warning "'strncmp' of a string of length 5, an array of size 4 and bound of 5 evaluates to nonzero" }
  if (cmp)                          // { dg-message "in this expression" }
    sink (0, a4);

  // Verify no warning when the bound is the same as the array size.
  T (a4, "4321", 4);
  T (a4, "654321", 4);

  T (a4, "12345", 5);       // { dg-warning "length 5, an array of size 4 and bound of 5 " }
  T (a4, "123456", 6);      // { dg-warning "length 6, an array of size 4 and bound of 6" }

  T ("1234", a4, 4);
  T ("12345", a4, 4);

  T ("12345", a4, 5);       // { dg-warning "length 5, an array of size 4 and bound of 5 " }
  T ("123456", a4, 6);      // { dg-warning "length 6, an array of size 4 and bound of 6 " }
}


void strncmp_strarray_copy (void)
{
  {
    char a[] = "1234";
    char b[6];
    strcpy (b, "12345");
    if (strncmp (a, b, 5))  // { dg-warning "'strncmp' of strings of length 4 and 5 and bound of 5 evaluates to nonzero" }
                            // { dg-bogus "in this expreession" "unwanted note" { target *-*-* } .-1 }
      sink (0, a, b);
  }

  {
    char a[] = "4321";
    char b[6];
    strcpy (b, "54321");
    int cmp;
    cmp = strncmp (a, b, 5);  // { dg-warning "'strncmp' of strings of length 4 and 5 and bound of 5 evaluates to nonzero" }
    if (cmp)                  // { dg-message "in this expression" }
      sink (0, a, b);
  }

  strcpy (a4, "abc");
  T (a4, "54321", 5);       // { dg-warning "'strncmp' of strings of length 3 and 5 and bound of 5 evaluates to nonzero " }
}


