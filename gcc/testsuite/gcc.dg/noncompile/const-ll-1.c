/* Test for broken long long suffixes.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */

/* The following are valid integer suffixes, according to C99:

   no suffix
   u or U
   ul, uL, Ul or UL
   ull, uLL, Ull or ULL
   l or L
   lu, lU, Lu or LU
   llu, llU, LLu or LLU

   The following are not but have been accepted by GCC in the past:

   lul and case variants (the 'l's being separated by a 'u')
   lL, Ll and variants with a 'u' (mixed case pair of 'l's)

   (cpplib gets this right when processing #if expressions.)

*/

unsigned long long a = 1LUL; /* { dg-error "lul|LUL" "error for LUL suffix" } */
long long b = 1Ll; /* { dg-error "Ll" "error for Ll suffix" } */
