/* Unpatched, this file would include "inc/ foo.h" (note the space)  */

#define PREINC_XSTR(str)             #str
#define PREINC_STR(str)              PREINC_XSTR(str)
#define COMP_INC(comp,file)          PREINC_STR(comp/file)

#include COMP_INC(inc, foo.h)
