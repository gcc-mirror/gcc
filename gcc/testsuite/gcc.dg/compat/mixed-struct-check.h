/* Function definitions that are used by multiple tests.  */

void checkScd (Scd x, int i)
{ if (x.c != (char)i || x.d != (double)i+1) DEBUG_CHECK }
void checkScdc (Scdc x, int i)
{ if (x.c != (char)i || x.d != (double)i+1 || x.b != (char)i+2) DEBUG_CHECK }
void checkSd (Sd x, int i)
{ if (x.d != (double)i) DEBUG_CHECK }
void checkSdi (Sdi x, int i)
{ if (x.d != (double)i || x.i != i+1) DEBUG_CHECK }
void checkScsds (Scsds x, int i)
{ if (x.c != (char)i || x.sd.d != (double)i+1) DEBUG_CHECK }
void checkScsdsc (Scsdsc x, int i)
{ if (x.c != (char)i || x.sd.d != (double)i+1 || x.b != (char)i+2) DEBUG_CHECK }
void checkScsdis (Scsdis x, int i)
{ if (x.c != (char)i || x.sdi.d != (double)i+1 || x.sdi.i != i+2) DEBUG_CHECK }
void checkScsdisc (Scsdisc x, int i)
{ if (x.c != (char)i || x.sdi.d != (double)i+1 || x.sdi.i != i+2
    || x.b != (char)i+3) DEBUG_CHECK }
void checkSsds (Ssds x, int i)
{ if (x.sd.d != (double)i) DEBUG_CHECK }
void checkSsdsc (Ssdsc x, int i)
{ if (x.sd.d != (double)i || x.c != (char)i+1) DEBUG_CHECK }
void checkScssdss (Scssdss x, int i)
{ if (x.c != (char)i || x.ssds.sd.d != (double)i+1) DEBUG_CHECK }
void checkScssdssc (Scssdssc x, int i)
{ if (x.c != (char)i || x.ssds.sd.d != (double)i+1
    || x.b != (char)i+2) DEBUG_CHECK }

void checkSfi (Sfi x, int i)
{ if (x.f != (float)i || x.i != i+1) DEBUG_CHECK }
void checkSfii (Sfii x, int i)
{ if (x.f != (float)i || x.i1 != i+1 || x.i2 != i+2) DEBUG_CHECK }
void checkSfifi (Sfifi x, int i)
{ if (x.fi.f != (float)i || x.fi.i != i+1) DEBUG_CHECK }
void checkSfiifii (Sfiifii x, int i)
{ if (x.fii.f != (float)i || x.fii.i1 != i+1 || x.fii.i2 != i+2) DEBUG_CHECK }
