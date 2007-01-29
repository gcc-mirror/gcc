/* Use undocumented functions in libgcc to clear and test dummy floating
   point exception flags.  That functionality is in libgcc just for
   testing purposes.

   If fesetexcept and feclearexcept are available, use those instead.  */

/* Get names of exception flags.  */
#include <fenv.h>

extern void __dfp_clear_except (int);
#define DFP_CLEAR_EXCEPT(M) __dfp_clear_except(M)
extern int __dfp_test_except (int);
#define DFP_TEST_EXCEPT(M) __dfp_test_except(M)
