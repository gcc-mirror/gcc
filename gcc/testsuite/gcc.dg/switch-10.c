/* { dg-options "-O2 -fdump-tree-cfg" }  */
/* { dg-final { scan-tree-dump "case 0:" "cfg" } }  */
/* { dg-final { scan-tree-dump-not "case 1 ... 255:" "cfg" } }  */
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
