/* Reproducing this ICE is dependent on line numbering, hence the blank
   lines below.  */
#include "substring-location-PR-87562-1-a.h"




#include "substring-location-PR-87562-1-b.h"

void
dbxout_stab_value_internal_label (const char *stem, int counter)
{
  char label[100];
  ASM_GENERATE_INTERNAL_LABEL (label, stem, counter);
}
