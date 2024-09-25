/* CTF generation for enums.

   CTF represents enum values with an int32_t.  For enum values not
   representable with int32_t data type, the compiler skips adding CTF for
   them.  This will be fixed soon in the CTF format.
   TBD_CTF_REPRESENTATION_LIMIT.
   
   In this testcase, CTF for enumerator GFS_MONOTONIC will not be generated.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "ascii \"GFS_MONOTONIC.0\"\[\t \]+\[^\n\]*ctf_string" 0 } } */
/* { dg-final { scan-assembler-times "ascii \"GFS_RUNTIME.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"GFS_STATIC.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "cte_value" 2} } */


enum gomp_schedule_type
{
  GFS_RUNTIME,
  GFS_STATIC,
  GFS_MONOTONIC = 0x80000000U
};

enum gomp_schedule_type gsch_type;

