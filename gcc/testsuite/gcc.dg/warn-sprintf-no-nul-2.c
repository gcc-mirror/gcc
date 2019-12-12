/* PR tree-optimization/87756 - missing unterminated argument warning
   using address of a constant character
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

int sprintf (char*, const char*, ...);

extern char* dest (void);
extern void sink (int, ...);

#define D dest ()
#define T(expr)   sink (0, (expr))


const char cnul = '\0';
const char cnonul = 'a';
const char str3[] = "123";

const struct
{
  char a, b, s[3];
} s1 = { '\0', 'b', "123" },
  s2[2] = {
  { '\0', 'c', "12" },
  { 'd', '\0', "123" }
  };

void test_sprintf_s (void)
{
  T (sprintf (D, "%s", &cnul));
  T (sprintf (D, "%s", &cnonul));       /* { dg-warning "nul-terminated" } */
  T (sprintf (D, "%.1s", &cnonul));
  T (sprintf (D, "%.2s", &cnonul));     /* { dg-warning "nul-terminated" } */

  T (sprintf (D, "%s", &s1.a));
  T (sprintf (D, "%s", &s1.b));         /* { dg-warning "nul-terminated" } */
  T (sprintf (D, "%.1s", &s1.b));
  T (sprintf (D, "%.2s", &s1.b));       /* { dg-warning "nul-terminated" } */
  T (sprintf (D, "%s", s1.s));          /* { dg-warning "nul-terminated" } */
  T (sprintf (D, "%.3s", s1.s));
  T (sprintf (D, "%.4s", s1.s));        /* { dg-warning "nul-terminated" } */

  T (sprintf (D, "%.2s", s1.s + 1));
  T (sprintf (D, "%.3s", s1.s + 1));    /* { dg-warning "nul-terminated" } */

  T (sprintf (D, "%s", &s2[0].a));
  T (sprintf (D, "%s", &s2[0].b));      /* { dg-warning "nul-terminated" } */
  T (sprintf (D, "%.1s", &s2[0].b));
  T (sprintf (D, "%.2s", &s2[0].b));    /* { dg-warning "nul-terminated" } */
  T (sprintf (D, "%s", s2[0].s));
  T (sprintf (D, "%.3s", s2[0].s));
  T (sprintf (D, "%.4s", s2[0].s));

  T (sprintf (D, "%.2s", s2[0].s + 1));
  T (sprintf (D, "%.3s", s2[0].s + 1));

  T (sprintf (D, "%s", &s2[1].a));      /* { dg-warning "nul-terminated" } */
  T (sprintf (D, "%.1s", &s2[1].a));
  T (sprintf (D, "%.2s", &s2[1].a));    /* { dg-warning "nul-terminated" } */
  T (sprintf (D, "%s", &s2[1].b));
  T (sprintf (D, "%s", s2[1].s));       /* { dg-warning "nul-terminated" } */
  T (sprintf (D, "%.3s", s2[1].s));
  T (sprintf (D, "%.4s", s2[1].s));     /* { dg-warning "nul-terminated" } */

  T (sprintf (D, "%.2s", s2[1].s + 1));
  T (sprintf (D, "%.3s", s2[1].s + 1)); /* { dg-warning "nul-terminated" } */

  T (sprintf (D, "%s", &str3[3]));
  T (sprintf (D, "%s", &str3[4]));      /* { dg-warning "\\\[-Warray-bounds" } */
}
