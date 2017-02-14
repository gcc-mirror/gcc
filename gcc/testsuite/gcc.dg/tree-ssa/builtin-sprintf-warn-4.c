/* { dg-do compile } */
/* { dg-options "-Wformat -Wformat-overflow=1 -fdiagnostics-show-caret" } */

extern int sprintf (char*, const char*, ...);

char dst [3];

void test (void)
{
  /* Verify thet the caret points to the (invisible) nul character
     at the end of the format string (i.e., its closing quote).
     The redundant argument is there to get around GCC bug 77799.  */
  sprintf (dst + 2, "1", 0);
  /* { dg-warning "writing a terminating nul past the end of the destination" "nul warning" { target *-*-* } .-1 }
     { dg-message ".sprintf. output 2 bytes into a destination of size 1" "note" { target *-*-* } .-2 }
     { dg-begin-multiline-output "-Wformat output: redundant argument" }
   sprintf (dst + 2, "1", 0);
                     ^~~
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "-Wformat-overflow output" }
   sprintf (dst + 2, "1", 0);
                       ^
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "note" }
   sprintf (dst + 2, "1", 0);
   ^~~~~~~~~~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */

  /* Verify thet the caret points at the first format character written
     past the end of the destination.  */
  sprintf (dst, "1234", 0);
  /* { dg-warning "writing 4 bytes into a region of size 3" "overlong format string" { target *-*-* } .-1 }
     { dg-message ".sprintf. output 5 bytes into a destination of size 3" "note" { target *-*-* } .-2 }
     { dg-begin-multiline-output "-Wformat output: redundant argument" }
   sprintf (dst, "1234", 0);
                 ^~~~~~
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "-Wformat-overflow output" }
   sprintf (dst, "1234", 0);
                  ~~~^
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "note" }
   sprintf (dst, "1234", 0);
   ^~~~~~~~~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */

  /* Verify thet the caret points at the first format character written
     past the end of the destination and the rest of the format string
     is underlined.  */
  sprintf (dst, "12345", 0);
  /* { dg-warning "writing 5 bytes into a region of size 3" "nul warning" { target *-*-* } .-1 }
     { dg-message ".sprintf. output 6 bytes into a destination of size 3" "note" { target *-*-* } .-2 }
     { dg-begin-multiline-output "-Wformat output: redundant argument" }
   sprintf (dst, "12345", 0);
                 ^~~~~~~
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "-Wformat-overflow output" }
   sprintf (dst, "12345", 0);
                  ~~~^~
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "note" }
   sprintf (dst, "12345", 0);
   ^~~~~~~~~~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */

  /* Same as above but with a directive.  The minus flag is used to
     get around GCC bug 77671.  */
  sprintf (dst + 2, "%-s", "1");
  /* { dg-warning "writing a terminating nul past the end of the destination" "warning" { target *-*-* } .-1 }
     { dg-message ".sprintf. output 2 bytes into a destination of size 1" "note" { target *-*-* } .-2 }
     { dg-begin-multiline-output "-Wformat-overflow output" }
   sprintf (dst + 2, "%-s", "1");
                         ^
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "note" }
   sprintf (dst + 2, "%-s", "1");
   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  sprintf (dst + 2, "%-s", "abcd");
  /* { dg-warning ".%-s. directive writing 4 bytes into a region of size 1" "warning" { target *-*-* } .-1 }
     { dg-message ".sprintf. output 5 bytes into a destination of size 1" "note" { target *-*-* } .-2 }
     { dg-begin-multiline-output "-Wformat-overflow output" }
   sprintf (dst + 2, "%-s", "abcd");
                      ^~~   ~~~~~~
     { dg-end-multiline-output "" }
     { dg-begin-multiline-output "note" }
   sprintf (dst + 2, "%-s", "abcd");
   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    { dg-end-multiline-output "" } */
}

/* { dg-prune-output "too many arguments for format" } */

/* When debugging, define LINE to the line number of the test case to exercise
   and avoid exercising any of the others.  The buffer macro
   below makes use of LINE to avoid warnings for other lines.  */
#ifndef LINE
# define LINE 0
#endif

char buffer [256];
extern char *ptr;

/* Evaluate to an array of SIZE characters when non-negative and LINE
   is not set or set to the line the macro is on, or to a pointer to
   an unknown object otherwise.  */
#define buffer(size)                                                    \
  (0 <= size && (!LINE || __LINE__ == LINE)                             \
   ? buffer + sizeof buffer - size : ptr)

/* Verify that the note printed along with the diagnostic mentions
   the correct sizes and refers to the location corresponding to
   the affected directive.  */

void test_sprintf_note (void)
{
  /* Diagnostic column numbers are 1-based.  */

  __builtin_sprintf (buffer (0), "%c%s%i", '1', "2", 3);
  /* { dg-warning "35: .%c. directive writing 1 byte into a region of size 0" "" { target *-*-* } .-1 }
     { dg-begin-multiline-output "" }
   __builtin_sprintf (buffer (0), "%c%s%i", '1', "2", 3);
                                   ^~
     { dg-end-multiline-output "" }

     { dg-message ".__builtin_sprintf. output 4 bytes into a destination of size 0" "" { target *-*-* } .-7 }
     { dg-begin-multiline-output "" }
   __builtin_sprintf (buffer (0), "%c%s%i", '1', "2", 3);
   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */

  __builtin_sprintf (buffer (1), "%c%s%i", '1', "23", 45);
  /* { dg-warning "37: .%s. directive writing 2 bytes into a region of size 0" "" { target *-*-* } .-1 }
     { dg-begin-multiline-output "" }
   __builtin_sprintf (buffer (1), "%c%s%i", '1', "23", 45);
                                     ^~          ~~~~
     { dg-end-multiline-output "" }

     { dg-message ".__builtin_sprintf. output 6 bytes into a destination of size 1" "" { target *-*-* } .-7 }
     { dg-begin-multiline-output "" }
   __builtin_sprintf (buffer (1), "%c%s%i", '1', "23", 45);
   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */

  __builtin_sprintf (buffer (2), "%c%s%i", '1', "2", 345);
  /* { dg-warning "39: .%i. directive writing 3 bytes into a region of size 0" "" { target *-*-* } .-1 }
     { dg-begin-multiline-output "" }
   __builtin_sprintf (buffer (2), "%c%s%i", '1', "2", 345);
                                       ^~
     { dg-end-multiline-output "" }

     { dg-message ".__builtin_sprintf. output 6 bytes into a destination of size 2" "" { target *-*-* } .-7 }
     { dg-begin-multiline-output "" }
   __builtin_sprintf (buffer (2), "%c%s%i", '1', "2", 345);
   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */

  __builtin_sprintf (buffer (6), "%c%s%i", '1', "2", 3456);
  /* { dg-warning "41: writing a terminating nul past the end of the destination" "" { target *-*-* } .-1 }
     { dg-begin-multiline-output "" }
   __builtin_sprintf (buffer (6), "%c%s%i", '1', "2", 3456);
                                         ^
     { dg-end-multiline-output "" }

     { dg-message ".__builtin_sprintf. output 7 bytes into a destination of size 6" "" { target *-*-* } .-7 }
     { dg-begin-multiline-output "" }
   __builtin_sprintf (buffer (6), "%c%s%i", '1', "2", 3456);
   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */
}
