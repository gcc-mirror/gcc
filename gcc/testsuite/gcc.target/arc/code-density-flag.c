/* Produce code-dense instructions  and  the assembler must *
 * be okay with it. An example would be:                    *
 *                                                          *
 * sub_s  r3, r1, r3                                        *
 *                                                          *
 * While generally for  _short instructions_ ,  it  is  not *
 * allowed to have different registers  as  the  first  and *
 * second operands, the code-dense mode allows it.          *
 * This test is about the fact that if  "-mcode-density" is *
 * passed to gcc driver as the flag,  "as"  must receive it *
 * as well, else it is going to choke on such encodings.    */

/* { dg-do assemble }                                       */
/* { dg-skip-if "" { ! { clmcpu } } }                       */
/* { dg-options "-mcpu=em_mini -mcode-density" }            */

typedef long long uint64_t;

uint64_t f1(void)
{
  return 1;
}

void f2(void)
{
  uint64_t start_us = 0;
  while ((f1() - start_us) < 2);
}

/* This is a tricky check, because  it  hardcodes  register *
 * numbers. Nevertheless, it is easier than coming up  with *
 * a regular expression that the first two operands  should *
 * not be the same.                                         */
/* { dg-final { scan-assembler "sub_s\\s+r3,r1,r3" } }      */
