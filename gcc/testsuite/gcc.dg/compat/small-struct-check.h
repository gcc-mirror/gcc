/* Function definitions that are used by multiple tests.  */

void checkSc (Sc x, int i) { if (x.c != (char)i) DEBUG_CHECK }
void checkSs (Ss x, int i) { if (x.s != i) DEBUG_CHECK }
void checkSi (Si x, int i) { if (x.i != i) DEBUG_CHECK }
void checkSsc (Ssc x, int i)
{ if (x.s != i || x.c != (char)i+1) DEBUG_CHECK }
void checkScs (Scs x, int i)
{ if (x.c != (char)i || x.s != i+1) DEBUG_CHECK }
void checkSsi (Ssi x, int i)
{ if (x.s != i || x.i != i+1) DEBUG_CHECK }
void checkSis (Sis x, int i)
{ if (x.i != i || x.s != i+1) DEBUG_CHECK }
void checkSic (Sic x, int i)
{ if (x.i != i || x.c != (char)i+1) DEBUG_CHECK }
void checkSci (Sci x, int i)
{ if (x.c != (char)i || x.i != i+1) DEBUG_CHECK }
void checkScsi (Scsi x, int i)
{ if (x.c != (char)i || x.s != i+1 || x.i != i+2) DEBUG_CHECK }
void checkScis (Scis x, int i)
{ if (x.c != (char)i || x.i != i+1 || x.s != i+2) DEBUG_CHECK }
void checkSsci (Ssci x, int i)
{ if (x.s != i || x.c != (char)i+1 || x.i != i+2) DEBUG_CHECK }
void checkSsic (Ssic x, int i)
{ if (x.s != i || x.i != i+1 || x.c != (char)i+2) DEBUG_CHECK }
void checkSisc (Sisc x, int i)
{ if (x.i != i || x.s != i+1 || x.c != (char)i+2) DEBUG_CHECK }
void checkSics (Sics x, int i)
{ if (x.i != i || x.c != (char)i+1 || x.s != i+2) DEBUG_CHECK }
