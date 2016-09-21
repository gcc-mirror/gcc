/* { dg-do compile } */
/* { dg-options "-Wformat -Wformat-length=1 -fdiagnostics-show-caret -ftrack-macro-expansion=0" } */

extern int sprintf (char*, const char*, ...);

char dst [8];

void test (void)
{
  sprintf (dst + 7, "%-s", "1");
  /* { dg-warning "writing a terminating nul past the end of the destination" "" { target *-*-*-* } 10 }
     { dg-message "format output 2 bytes into a destination of size 1" "" { target *-*-*-* } 10 }
     { dg-begin-multiline-output "" }
   sprintf (dst + 7, "%-s", "1");
                     ^~~~~
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "" }
   sprintf (dst + 7, "%-s", "1");
   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  sprintf (dst + 7, "%-s", "abcd");
  /* { dg-warning ".%-s. directive writing 4 bytes into a region of size 1" "" { target *-*-*-* } 22 }
     { dg-message "format output 5 bytes into a destination of size 1" "" { target *-*-*-* } 22 }
     { dg-begin-multiline-output "" }
   sprintf (dst + 7, "%-s", "abcd");
                      ^~~   ~~~~~~
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "" }
   sprintf (dst + 7, "%-s", "abcd");
   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    { dg-end-multiline-output "" } */
}
