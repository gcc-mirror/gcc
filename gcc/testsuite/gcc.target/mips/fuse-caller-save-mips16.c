/* { dg-do compile } */
/* { dg-options "-fipa-ra (-mips16) addressing=absolute isa_rev=0" } */
/* At -O0 and -O1, the register allocator behaves more conservatively, and
   the fipa-ra optimization doesnt' trigger.  */
/* { dg-skip-if "" { *-*-* }  { "-O0" "-O1" } } */
/* Testing -fipa-ra optimization option.  */

#define ATTRIBUTE MIPS16
#include "fuse-caller-save.h"

/* Check that there are only 2 stack-saves: r31 in main and foo.  */

/* Check that there only 2 sw/sd.  */
/* { dg-final { scan-assembler-times "(?n)s\[wd\]\t\\\$.*,.*\\(\\\$sp\\)" 2 } } */

/* Check that the first caller-save register is unused.  */
/* { dg-final { scan-assembler-not "\\\$16" } } */
