/* String initializers for arrays must not be parenthesized.  Bug
   11250 from h.b.furuseth at usit.uio.no.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

#include <stddef.h>

char *a = "a";
char *b = ("b");
char *c = (("c"));

char d[] = "d";
char e[] = ("e"); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "parenthesized|near init" "paren array" { target *-*-* } 14 } */
char f[] = (("f")); /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "parenthesized|near init" "paren array" { target *-*-* } 16 } */

signed char g[] = { "d" };
unsigned char h[] = { ("e") }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "parenthesized|near init" "paren array" { target *-*-* } 20 } */
signed char i[] = { (("f")) }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "parenthesized|near init" "paren array" { target *-*-* } 22 } */


struct s { char a[10]; int b; wchar_t c[10]; };

struct s j = {
  "j",
  1,
  (L"j")
}; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "parenthesized|near init" "paren array" { target *-*-* } 32 } */
struct s k = {
  (("k")), /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "parenthesized|near init" "paren array" { target *-*-* } 35 } */
  1,
  L"k"
};

struct s l = {
  .c = (L"l"), /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "parenthesized|near init" "paren array" { target *-*-* } 42 } */
  .a = "l"
};

struct s m = {
  .c = L"m",
  .a = ("m")
}; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "parenthesized|near init" "paren array" { target *-*-* } 50 } */

char *n = (char []){ "n" };

char *o = (char []){ ("o") }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "parenthesized|near init" "paren array" { target *-*-* } 55 } */

wchar_t *p = (wchar_t [5]){ (L"p") }; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "parenthesized|near init" "paren array" { target *-*-* } 58 } */
