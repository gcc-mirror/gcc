/* In Sony versions before 3.0, use the GNU Assembler, because the
   system's assembler has no way to assemble the difference of two
   labels for the displacement in a switch-dispatch instruction.  */  

#define USE_GAS

#include "news.h"
