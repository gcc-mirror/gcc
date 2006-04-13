/* { dg-require-effective-target int32plus } */
#include "struct-1.h"

struct ChainSearchRecord {
 int identity;
};
typedef struct ChainSearchRecord ChainSearchRecord;
void foo (ChainSearchPtr s)
{
  s->identity = 0x6a73616d;
}

