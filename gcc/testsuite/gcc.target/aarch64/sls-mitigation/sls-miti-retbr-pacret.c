/* Avoid ILP32 since pacret is only available for LP64 */
/* { dg-do compile { target { ! ilp32 } } } */
/* { dg-additional-options "-mharden-sls=retbr -mbranch-protection=pac-ret -march=armv8.3-a" } */

/* Testing the do_return pattern for retaa and retab.  */
long retbr_subcall(void);
long retbr_do_return_retaa(void)
{
    return retbr_subcall()+1;
}

__attribute__((target("branch-protection=pac-ret+b-key")))
long retbr_do_return_retab(void)
{
    return retbr_subcall()+1;
}

/* Ensure there are no BR or RET instructions which are not directly followed
   by a speculation barrier.  */
/* { dg-final { scan-assembler-not {\t(br|ret|retaa|retab)\tx[0-9][0-9]?\n\t(?!dsb\tsy\n\tisb)} } } */
/* { dg-final { scan-assembler-not {ret\t} } } */
