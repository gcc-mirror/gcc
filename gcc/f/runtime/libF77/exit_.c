/* This gives the effect of

	subroutine exit(rc)
	integer*4 rc
	stop
	end

 * with the added side effect of supplying rc as the program's exit code.
 */

#include "f2c.h"
#undef abs
#undef min
#undef max
#ifndef KR_headers
#include <stdlib.h>
#ifdef __cplusplus
extern "C" {
#endif
extern void f_exit(void);
#endif

 void
#ifdef KR_headers
G77_exit_0 (rc) integer *rc;
#else
G77_exit_0 (integer *rc)
#endif
{
#ifdef NO_ONEXIT
	f_exit();
#endif
	exit(*rc);
	}
#ifdef __cplusplus
}
#endif
