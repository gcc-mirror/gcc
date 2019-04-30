/* PR c++/90010 */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

char b[4096] = "abc";
void bar (char *);

void
foo ()
{
  char d[4096];
  __builtin_snprintf (d, sizeof d, "%sfoobarbazquxquuxquuzthudfred", b);	/* { dg-warning "'foobarbazquxquuxquuzthudfred' directive output may be truncated writing 28 bytes into a region of size between 1 and 4096" } */
  /* { dg-message "'__builtin_snprintf' output between 29 and 4124 bytes into a destination of size 4096" "" { target *-*-* } .-1 } */
  bar (d);
  __builtin_snprintf (d, sizeof d, "%sfoobarbazquxquuxquuzcorgefred", b);	/* { dg-warning "'foobarbazquxquuxquuzcorgefred' directive output may be truncated writing 29 bytes into a region of size between 1 and 4096" } */
  /* { dg-message "'__builtin_snprintf' output between 30 and 4125 bytes into a destination of size 4096" "" { target *-*-* } .-1 } */
  bar (d);
  __builtin_snprintf (d, sizeof d, "%sfoobarbazquxquuxquuzcorgewaldo", b);	/* { dg-warning "'foobarbazquxquuxquuzcorgewaldo' directive output may be truncated writing 30 bytes into a region of size between 1 and 4096" } */
  /* { dg-message "'__builtin_snprintf' output between 31 and 4126 bytes into a destination of size 4096" "" { target *-*-* } .-1 } */
  bar (d);
  __builtin_snprintf (d, sizeof d, "%sfoobarbazquxquuxquuzcorgegarply", b);	/* { dg-warning "'foobarbazquxquuxquuzcorgegarply' directive output may be truncated writing 31 bytes into a region of size between 1 and 4096" } */
  /* { dg-message "'__builtin_snprintf' output between 32 and 4127 bytes into a destination of size 4096" "" { target *-*-* } .-1 } */
  bar (d);
  __builtin_snprintf (d, sizeof d, "%sfoobarfredquxquuxquuzcorgegarply", b);	/* { dg-warning "'foobarfredquxquuxquuzcorgega\.\.\.' directive output may be truncated writing 32 bytes into a region of size between 1 and 4096" } */
  /* { dg-message "'__builtin_snprintf' output between 33 and 4128 bytes into a destination of size 4096" "" { target *-*-* } .-1 } */
  bar (d);
}
