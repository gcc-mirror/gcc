/* Host environment for 68000's running System V.  */

#include "m68k/xm-m68k.h"

#define USG
#ifndef __GNUC__
#define USE_C_ALLOCA
#endif

#ifndef _SIZE_T_
typedef int size_t;
#define _SIZE_T_
#endif
