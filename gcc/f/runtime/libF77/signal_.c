#include "f2c.h"
#include "signal1.h"

#ifdef KR_headers
sig_pf G77_signal_0 (sigp, proc) integer *sigp; sig_pf proc;
#else
sig_pf G77_signal_0 (integer *sigp, sig_pf proc)
#endif
{
	int sig;
	sig = (int)*sigp;

	return signal(sig, proc);
	}
