/* { dg-do compile } */
/* { dg-options "-Wformat -Wformat-length=1 -fdiagnostics-show-caret -ftrack-macro-expansion=0" } */

extern int sprintf (char*, const char*, ...);

char dst [3];

void test (void)
{
  /* Verify thet the caret points to the (invisible) nul character
     at the end of the format string (i.e., its closing quote).
     The redundant argument is there to get around GCC bug 77799.  */
  sprintf (dst + 2, "1", 0);
  /* { dg-warning "writing a terminating nul past the end of the destination" "nul warning" { target *-*-* } .-1 }
     { dg-message "format output 2 bytes into a destination of size 1" "note" { target *-*-* } .-2 }
     { dg-begin-multiline-output "-Wformat output: redundant argument" }
   sprintf (dst + 2, "1", 0);
                     ^~~
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "-Wformat-length output" }
   sprintf (dst + 2, "1", 0);
                      ~^
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "note" }
   sprintf (dst + 2, "1", 0);
   ^~~~~~~~~~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */

  /* Verify thet the caret points at the first format character written
     past the end of the destination.  */
  sprintf (dst, "1234", 0);
  /* { dg-warning "writing format character .4. at offset 3 past the end of the destination" "nul warning" { target *-*-* } .-1 }
     { dg-message "format output 5 bytes into a destination of size 3" "note" { target *-*-* } .-2 }
     { dg-begin-multiline-output "-Wformat output: redundant argument" }
   sprintf (dst, "1234", 0);
                 ^~~~~~
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "-Wformat-length output" }
   sprintf (dst, "1234", 0);
                     ^
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "note" }
   sprintf (dst, "1234", 0);
   ^~~~~~~~~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */

  /* Verify thet the caret points at the first format character written
     past the end of the destination and the rest of the format string
     is underlined.  */
  sprintf (dst, "12345", 0);
  /* { dg-warning "writing format character .4. at offset 3 past the end of the destination" "nul warning" { target *-*-* } .-1 }
     { dg-message "format output 6 bytes into a destination of size 3" "note" { target *-*-* } .-2 }
     { dg-begin-multiline-output "-Wformat output: redundant argument" }
   sprintf (dst, "12345", 0);
                 ^~~~~~~
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "-Wformat-length output" }
   sprintf (dst, "12345", 0);
                     ^~
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "note" }
   sprintf (dst, "12345", 0);
   ^~~~~~~~~~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */

  /* Same as above but with a directive.  The minus flag is used to
     get around GCC bug 77671.  */
  sprintf (dst + 2, "%-s", "1");
  /* { dg-warning "writing a terminating nul past the end of the destination" "warning" { target *-*-* } .-1 }
     { dg-message "format output 2 bytes into a destination of size 1" "note" { target *-*-* } .-2 }
     { dg-begin-multiline-output "-Wformat-length output" }
   sprintf (dst + 2, "%-s", "1");
                      ~~~^
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "note" }
   sprintf (dst + 2, "%-s", "1");
   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  sprintf (dst + 2, "%-s", "abcd");
  /* { dg-warning ".%-s. directive writing 4 bytes into a region of size 1" "warning" { target *-*-* } .-1 }
     { dg-message "format output 5 bytes into a destination of size 1" "note" { target *-*-* } .-2 }
     { dg-begin-multiline-output "-Wformat-length output" }
   sprintf (dst + 2, "%-s", "abcd");
                      ^~~   ~~~~~~
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "note" }
   sprintf (dst + 2, "%-s", "abcd");
   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    { dg-end-multiline-output "" } */
}

/* { dg-prune-output "too many arguments for format" } */
