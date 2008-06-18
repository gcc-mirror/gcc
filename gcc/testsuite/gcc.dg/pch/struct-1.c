#include "struct-1.h"

struct ChainSearchRecord {
 int identity;
};
typedef struct ChainSearchRecord ChainSearchRecord;
void foo (ChainSearchPtr s)
{
#if(__SIZEOF_INT__ >= 4)	
  s->identity = 0x6a73616d;
#else
   s->identity = 0x616d;
#endif
}


