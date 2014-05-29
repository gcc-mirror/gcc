/* { dg-do compile } */
/* { dg-options "-fuse-caller-save" } */
/* At -O0 and -O1, the register allocator behaves more conservatively, and
   the fuse-caller-save optimization doesnt' trigger.  */
/* { dg-skip-if "" { *-*-* }  { "-O0" "-O1" } } */
/* Testing -fuse-caller-save optimization option.  */

#define ATTRIBUTE NOCOMPRESSION
#include "fuse-caller-save.h"

/* Check that there are only 2 stack-saves: r31 in main and foo.  */

/* Check that there only 2 sw/sd.  */
/* { dg-final { scan-assembler-times "(?n)s\[wd\]\t\\\$.*,.*\\(\\\$sp\\)" 2 } } */

/* Check that the first caller-save register is unused.  */
/* { dg-final { scan-assembler-not "\\\$16" } } */
