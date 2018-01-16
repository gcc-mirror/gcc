/* PR c/83448 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wformat-truncation -fdiagnostics-show-caret" } */

extern int snprintf (char *, __SIZE_TYPE__, const char *, ...);

void
foo (char *a, char *b, char *c, int d, int e)
{
  snprintf (a, 7, "abc\\\123 efg");
  /* { dg-warning "directive output truncated writing 9 bytes into a region of size 7" "" { target *-*-* } .-1 }
     { dg-message ".snprintf. output 10 bytes into a destination of size 7" "note" { target *-*-* } .-2 }
     { dg-begin-multiline-output "" }
   snprintf (a, 7, "abc\\\123 efg");
                    ~~~~~~~~~~~^~
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "note" }
   snprintf (a, 7, "abc\\\123 efg");
   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */
  d &= 63;
  d += 10;
  snprintf (b, 7, "a%dbcdefg", d);
  /* { dg-warning "'bcdefg' directive output truncated writing 6 bytes into a region of size 4" "" { target *-*-* } .-1 }
     { dg-message ".snprintf. output 10 bytes into a destination of size 7" "note" { target *-*-* } .-2 }
     { dg-begin-multiline-output "" }
   snprintf (b, 7, "a%dbcdefg", d);
                       ~~~~^~
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "note" }
   snprintf (b, 7, "a%dbcdefg", d);
   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */
  e &= 127;
  snprintf (c, 7, "a%dbcdefgh", e);
  /* { dg-warning "'bcdefgh' directive output truncated writing 7 bytes into a region of size between 3 and 5" "" { target *-*-* } .-1 }
     { dg-message ".snprintf. output between 10 and 12 bytes into a destination of size 7" "note" { target *-*-* } .-2 }
     { dg-begin-multiline-output "" }
   snprintf (c, 7, "a%dbcdefgh", e);
                       ~~~~~^~
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "note" }
   snprintf (c, 7, "a%dbcdefgh", e);
   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */
}
