/* Regression test for considering impossible path due to mishandling of
   bitfields (PR analyzer/109197).  */

#include "analyzer-decls.h"

struct a
{
  int d : 10;
} b;

void e()
{
  b.d = 0;
  if (b.d)
    __analyzer_dump_path (); /* { dg-bogus "path" "PR analyzer/109197" } */
}
