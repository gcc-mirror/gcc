/* The bitfield type (int) may be shared, but slices are not de-duplicated.

   In this testcase, it is expected to see a total of 6 CTF slices and 2 CTF
   integer types for the bitfields - unsigned long long and signed long long.
   
   cts_offset is the offset of the bitfield into a machine word.
   TBD - hardcoding cts_offset checks into the testcase will cause it to break
   across targets with different BIT_PER_WORD.  Is there a way to add
   cts_offset related checks in the testcase?  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x6\[\t \]+\[^\n\]*cts_type" 3 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x2\[\t \]+\[^\n\]*cts_type" 3 } } */

/* { dg-final { scan-assembler-times "\[\t \]0xf\[\t \]+\[^\n\]*cts_bits" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x21\[\t \]+\[^\n\]*cts_bits" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x10\[\t \]+\[^\n\]*cts_bits" 2 } } */

/* { dg-final { scan-assembler-times "ascii \"long long unsigned int.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"long long int.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

struct fields
{
  unsigned long long u1 : 15;
  unsigned long long u2 : 33;
  unsigned long long u3 : 16;
  signed long long   s1 : 15;
  signed long long   s2 : 33;
  signed long long   s3 : 16;
} flags;

int i = 33;

int main ()
{
    return flags.u1 + i;
}
