/* Configuration for an i386 running GNU as the target machine.  */

/* We do want to add an underscore to the front of each user symbol.
   i386/gas.h checks this.  */
#define YES_UNDERSCORES

#include <i386/gstabs.h>

/* Get perform_* macros to build libgcc.a.  */
#include <i386/perform.h>

#undef CPP_PREDEFINES
#define CPP_PREDEFINES GNU_CPP_PREDEFINES("i386")

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Get machine-independent configuration parameters for the GNU system.  */
#include <gnu.h>
