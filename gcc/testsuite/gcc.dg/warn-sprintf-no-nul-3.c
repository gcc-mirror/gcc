/* PR tree-optimization/87756 - missing unterminated argument warning
   using address of a constant character
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

typedef __WCHAR_TYPE__ wchar_t;

int sprintf (char*, const char*, ...);

extern char* dest (void);
extern void sink (int, ...);

#define D dest ()
#define T(expr)   sink (0, (expr))

const wchar_t wnul = L'\0';
const wchar_t wnonul = L'a';
const wchar_t wcs3[] = L"123";

const struct
{
  wchar_t a, b, s[3];
} w1 = { L'\0', L'b', L"123" },
  w2[2] = {
  { L'\0', L'c', L"12" },
  { L'd', L'\0', L"123" }
  };

void test_sprintf_ls (void)
{
  T (sprintf (D, "%ls", &wnul));
  T (sprintf (D, "%ls", &wnonul));      /* { dg-warning "nul-terminated" } */
  T (sprintf (D, "%.1ls", &wnonul));
  T (sprintf (D, "%.2ls", &wnonul));    /* { dg-warning "nul-terminated" } */

  T (sprintf (D, "%ls", &w1.a));
  T (sprintf (D, "%ls", &w1.b));        /* { dg-warning "nul-terminated" "pr88211" { xfail *-*-* } } */
  T (sprintf (D, "%.1ls", &w1.b));
  T (sprintf (D, "%.2ls", &w1.b));      /* { dg-warning "nul-terminated" "pr88211" { xfail *-*-* } } */
  T (sprintf (D, "%ls", w1.s));         /* { dg-warning "nul-terminated" } */
  T (sprintf (D, "%.3ls", w1.s));
  T (sprintf (D, "%.4ls", w1.s));       /* { dg-warning "nul-terminated" } */

  T (sprintf (D, "%.2ls", w1.s + 1));
  T (sprintf (D, "%.3ls", w1.s + 1));   /* { dg-warning "nul-terminated" } */

  T (sprintf (D, "%ls", &w2[0].a));
  T (sprintf (D, "%ls", &w2[0].b));     /* { dg-warning "nul-terminated" "pr88211" { xfail *-*-* } } */
  T (sprintf (D, "%.1ls", &w2[0].b));
  T (sprintf (D, "%.2ls", &w2[0].b));   /* { dg-warning "nul-terminated" "pr88211" { xfail *-*-* } } */
  T (sprintf (D, "%ls", w2[0].s));
  T (sprintf (D, "%.3ls", w2[0].s));
  T (sprintf (D, "%.4ls", w2[0].s));

  T (sprintf (D, "%.2ls", w2[0].s + 1));
  T (sprintf (D, "%.3ls", w2[0].s + 1));

  T (sprintf (D, "%ls", &w2[1].a));     /* { dg-warning "nul-terminated" "pr88211" { xfail *-*-* } } */
  T (sprintf (D, "%.1ls", &w2[1].a));
  T (sprintf (D, "%.2ls", &w2[1].a));   /* { dg-warning "nul-terminated" "pr88211" { xfail *-*-* } } */
  T (sprintf (D, "%ls", &w2[1].b));
  T (sprintf (D, "%ls", w2[1].s));      /* { dg-warning "nul-terminated" } */
  T (sprintf (D, "%.3ls", w2[1].s));
  T (sprintf (D, "%.4ls", w2[1].s));    /* { dg-warning "nul-terminated" } */

  T (sprintf (D, "%.2ls", w2[1].s + 1));
  T (sprintf (D, "%.3ls", w2[1].s + 1));/* { dg-warning "nul-terminated" } */

  T (sprintf (D, "%ls", &wcs3[3]));
  T (sprintf (D, "%ls", &wcs3[4]));     /* { dg-warning "\\\[-Warray-bounds" } */
}
