/* CTF generation for unsized arrays.

   Unsized arrays are encoded with a 0 for the number of elements.  The type
   of array index is the INT type.

   TBD_CTF_FORMAT_OPEN_ISSUES (1) - 
   This testcase makes a note of the case of a probable misrepresentation.
   See Note 1 below.

   In the CTF section, these types are encoded as :

     Variables:
      b1 ->  3: int [0] (size 0x0)
      b2 ->  5: int [0] (size 0x0)

    Note 1 : There is misrepresentation in that b1 and b2 are specified
    differently by the user.
    
    In this testcase, two CTF array records each of type int [0] is expected.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "0x12000000\[\t \]+\[^\n\]*ctt_info" 2 } } */

/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*cta_nelems" 2 } } */

static int b1[] = {};

int b2[0];
