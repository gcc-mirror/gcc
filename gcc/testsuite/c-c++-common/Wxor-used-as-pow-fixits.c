/* { dg-options "-fdiagnostics-show-caret" } */

/* Test fixit hints for -Wxor-used-as-pow.  */

int t2_8 = 2^8; /* { dg-line line_a } */
/* { dg-warning "result of '2\\^8' is 10; did you mean '1 << 8' \\(256\\)\\?" "warn" { target *-*-* } line_a } */
/* { dg-begin-multiline-output "" }
 int t2_8 = 2^8;
             ^
            --
            1<<
   { dg-end-multiline-output "" } */
/* { dg-message "you can silence this warning by using a hexadecimal constant \\(0x2 rather than 2\\)" "note" { target *-*-* } line_a } */
/* { dg-begin-multiline-output "" }
 int t2_8 = 2^8;
            ^
            0x2
   { dg-end-multiline-output "" } */


int t10_6 = 10^6; /* { dg-line line_b } */
/* { dg-warning "result of '10\\^6' is 12; did you mean '1e6'\\?"  "warn" { target *-*-* } line_b } */
/* { dg-begin-multiline-output "" }
 int t10_6 = 10^6;
               ^
             ---
             1e
   { dg-end-multiline-output "" } */
/* { dg-message "you can silence this warning by using a hexadecimal constant \\(0xa rather than 10\\)" "note" { target *-*-* } line_b } */
/* { dg-begin-multiline-output "" }
 int t10_6 = 10^6;
             ^~
             0xa
   { dg-end-multiline-output "" } */
