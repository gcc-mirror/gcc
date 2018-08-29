/* PR c/84293 unexpected warning from system header.  */
#include "./pr84293.h"
struct typeobject thing;

#pragma GCC diagnostic warning "-Wstrict-aliasing"
void __attribute__ ((optimize (2))) init ()
{
  INCREF_TDEF (&thing);
  INCREF_STAG (&thing);
}
