#include "f2c.h"

#ifdef KR_headers
extern integer s_cmp();
shortlogical hl_ge(a,b,la,lb) char *a, *b; ftnlen la, lb;
#else
extern integer s_cmp(char *, char *, ftnlen, ftnlen);
shortlogical hl_ge(char *a, char *b, ftnlen la, ftnlen lb)
#endif
{
return(s_cmp(a,b,la,lb) >= 0);
}
