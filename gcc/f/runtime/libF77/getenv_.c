#include "f2c.h"

/*
 * getenv - f77 subroutine to return environment variables
 *
 * called by:
 *	call getenv (ENV_NAME, char_var)
 * where:
 *	ENV_NAME is the name of an environment variable
 *	char_var is a character variable which will receive
 *		the current value of ENV_NAME, or all blanks
 *		if ENV_NAME is not defined
 */

#ifdef KR_headers
VOID G77_getenv_0 (fname, value, flen, vlen) char *value, *fname; ftnlen vlen, flen;
#else
void G77_getenv_0 (char *fname, char *value, ftnlen flen, ftnlen vlen)
#endif
{
extern char **environ;
register char *ep, *fp, *flast;
register char **env = environ;

flast = fname + flen;
for(fp = fname ; fp < flast ; ++fp)
	if(*fp == ' ')
		{
		flast = fp;
		break;
		}

while (ep = *env++)
	{
	for(fp = fname; fp<flast ; )
		if(*fp++ != *ep++)
			goto endloop;

	if(*ep++ == '=') {	/* copy right hand side */
		while( *ep && --vlen>=0 )
			*value++ = *ep++;

		goto blank;
		}
endloop: ;
	}

blank:
	while( --vlen >= 0 )
		*value++ = ' ';
}
