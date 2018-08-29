/* { dg-options "-O2 -fdump-tree-cfg" }  */
#include <stdint.h>

void foo (void);
void bar (void);

void
test (uint8_t ch)
{
  switch (ch)
   {
   case 0:
     foo ();
     break;

   case 1 ... 255:
     bar ();
     break;
   }
}

/* Switch statement is converted to GIMPLE condition.  */
/* { dg-final { scan-tree-dump-not "switch" "cfg" } }  */
