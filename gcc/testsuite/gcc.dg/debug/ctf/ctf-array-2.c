/* CTF generation for unsized arrays.

   Unsized arrays are encoded with a 0 for the number of elements.  The type
   of array index is the INT type.

   TBD_CTF_FORMAT_OPEN_ISSUES (1) - 
   This testcase makes a note of the case of a probable misrepresentation.
   See Note 1 and Note 2 below.

   In the CTF section, these types are encoded as :

     Variables:
      _CTF_NEWSTR ->  7: const char [0] (size 0x0)
      _CTF_SECTION ->  6: const char [5] (size 0x5)
      b1 ->  2: int [0] (size 0x0)
      b2 ->  3: int [0] (size 0x0)

    Note 1 : There is misrepresentation in that b1 and b2 are specified
    differently by the user.
    Note 2 : It is arguable though whether the representation for
    _CTF_NEWSTR is incorrect.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "0x12000000\[\t \]+\[^\n\]*ctt_info" 5 } } */

/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*cta_nelems" 3 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x5\[\t \]+\[^\n\]*cta_nelems" 1 } } */

static int b1[] = {};

int b2[0];

const char _CTF_SECTION[] = ".ctf";

extern const char _CTF_NEWSTR[];
const char _CTF_NEWSTR[] = "ctfinfo"; 
