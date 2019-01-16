/* PR tree-optimization/88444 */
/* { dg-do compile } */
/* { dg-options "-O1 -finline-functions -finline-small-functions -fdump-tree-fixup_cfg3" } */
/* { dg-final { scan-tree-dump-not " = \\(long int\\) 0;" "fixup_cfg3" } } */

#include "../pr88444.c"
