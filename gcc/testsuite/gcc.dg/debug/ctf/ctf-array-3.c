/* CTF generation for variable length arrays.

   In this testcase, a specific flavor of vla appears in the function
   signature.

   TBD_CTF_FORMAT_OPEN_ISSUES (1) -
   This testcase makes a note of another case of a probable misrepresentation.
   See ctf-array-2.c for some context on how vla's are a case of a probable
   misrepresentation in CTF.  Nevertheless, compilation should not fail.  */

/* { dg-do compile } */
/* { dg-options "-gctf" } */

int foo (int a, int b[a][a])
{
  return b[a-1][a-3];
}
