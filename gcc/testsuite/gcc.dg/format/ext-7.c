/* Test for scanf formats.  %a and %m extensions.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -Wformat" } */

#include "format.h"

void
foo (char **sp, wchar_t **lsp, int *ip, float *fp, void **pp, double *dp)
{
  /* %a formats for allocation, only recognized in C90 mode, are a
     GNU extension.  Followed by other characters, %a is not treated
     specially.
  */
  scanf ("%as", sp);
  scanf ("%aS", lsp);
  scanf ("%las", dp);
  scanf ("%la", lsp);	/* { dg-warning "but argument 2 has type" } */
  scanf ("%las", lsp);	/* { dg-warning "but argument 2 has type" } */
  scanf ("%a[bcd]", sp);
  scanf ("%la[bcd]", dp);
  scanf ("%*as");
  scanf ("%*aS");
  scanf ("%*las");	/* { dg-warning "assignment suppression and length modifier" } */
  scanf ("%*a[bcd]");
  scanf ("%*la[bcd]");	/* { dg-warning "assignment suppression and length modifier" } */
  scanf ("%10as", sp);
  scanf ("%5aS", lsp);
  scanf ("%9las", dp);
  scanf ("%25a[bcd]", sp);
  scanf ("%48la[bcd]", dp);
  scanf ("%*10as");
  scanf ("%*5aS");
  scanf ("%*9las");	/* { dg-warning "assignment suppression and length modifier" } */
  scanf ("%*25a[bcd]");
  scanf ("%*48la[bcd]"); /* { dg-warning "assignment suppression and length modifier" } */

  /* m assignment-allocation modifier, recognized in both C90
     and C99 modes, is a POSIX and ISO/IEC WDTR 24731-2 extension.  */
  scanf ("%ms", sp);
  scanf ("%mS", lsp);
  scanf ("%mls", lsp);
  scanf ("%m[bcd]", sp);
  scanf ("%ml[bcd]", lsp);
  scanf ("%mc", sp);
  scanf ("%mlc", lsp);
  scanf ("%mC", lsp);
  scanf ("%*ms");
  scanf ("%*mS");
  scanf ("%*mls");	/* { dg-warning "assignment suppression and length modifier" } */
  scanf ("%*m[bcd]");
  scanf ("%*ml[bcd]");	/* { dg-warning "assignment suppression and length modifier" } */
  scanf ("%*mc");
  scanf ("%*mlc");	/* { dg-warning "assignment suppression and length modifier" } */
  scanf ("%*mC");
  scanf ("%10ms", sp);
  scanf ("%5mS", lsp);
  scanf ("%9mls", lsp);
  scanf ("%25m[bcd]", sp);
  scanf ("%41ml[bcd]", lsp);
  scanf ("%131mc", sp);
  scanf ("%27mlc", lsp);
  scanf ("%2mC", lsp);
  scanf ("%*10ms");
  scanf ("%*5mS");
  scanf ("%*9mls");	/* { dg-warning "assignment suppression and length modifier" } */
  scanf ("%*25m[bcd]");
  scanf ("%*41ml[bcd]"); /* { dg-warning "assignment suppression and length modifier" } */
  scanf ("%*131mc");
  scanf ("%*27mlc");	/* { dg-warning "assignment suppression and length modifier" } */
  scanf ("%*2mC");

  scanf ("%md", ip);	/* { dg-warning "flag used with" } */
  scanf ("%mi", ip);	/* { dg-warning "flag used with" } */
  scanf ("%mo", ip);	/* { dg-warning "flag used with" } */
  scanf ("%mu", ip);	/* { dg-warning "flag used with" } */
  scanf ("%mx", ip);	/* { dg-warning "flag used with" } */
  scanf ("%me", fp);	/* { dg-warning "flag used with" } */
  scanf ("%mf", fp);	/* { dg-warning "flag used with" } */
  scanf ("%mg", fp);	/* { dg-warning "flag used with" } */
  scanf ("%mp", pp);	/* { dg-warning "flag used with" } */

  scanf ("%mas", sp);	/* { dg-warning "flag together" } */
  scanf ("%maS", lsp);	/* { dg-warning "flag together" } */
  scanf ("%ma[bcd]", sp);	/* { dg-warning "flag together" } */
}
