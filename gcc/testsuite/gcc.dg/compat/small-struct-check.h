/* Function definitions that are used by multiple tests.  */

void checkSc (Sc x, int i) { if (x.c != (char)i) { DEBUG_NL; abort (); } }
void checkSs (Ss x, int i) { if (x.s != i) { DEBUG_NL; abort (); } }
void checkSi (Si x, int i) { if (x.i != i) { DEBUG_NL; abort (); } }
void checkSsc (Ssc x, int i)
{ if (x.s != i || x.c != (char)i+1) { DEBUG_NL; abort (); } }
void checkScs (Scs x, int i)
{ if (x.c != (char)i || x.s != i+1) { DEBUG_NL; abort (); } }
void checkSsi (Ssi x, int i)
{ if (x.s != i || x.i != i+1) { DEBUG_NL; abort (); } }
void checkSis (Sis x, int i)
{ if (x.i != i || x.s != i+1) { DEBUG_NL; abort (); } }
void checkSic (Sic x, int i)
{ if (x.i != i || x.c != (char)i+1) { DEBUG_NL; abort (); } }
void checkSci (Sci x, int i)
{ if (x.c != (char)i || x.i != i+1) { DEBUG_NL; abort (); } }
void checkScsi (Scsi x, int i)
{ if (x.c != (char)i || x.s != i+1 || x.i != i+2) { DEBUG_NL; abort (); } }
void checkScis (Scis x, int i)
{ if (x.c != (char)i || x.i != i+1 || x.s != i+2) { DEBUG_NL; abort (); } }
void checkSsci (Ssci x, int i)
{ if (x.s != i || x.c != (char)i+1 || x.i != i+2) { DEBUG_NL; abort (); } }
void checkSsic (Ssic x, int i)
{ if (x.s != i || x.i != i+1 || x.c != (char)i+2) { DEBUG_NL; abort (); } }
void checkSisc (Sisc x, int i)
{ if (x.i != i || x.s != i+1 || x.c != (char)i+2) { DEBUG_NL; abort (); } }
void checkSics (Sics x, int i)
{ if (x.i != i || x.c != (char)i+1 || x.s != i+2) { DEBUG_NL; abort (); } }
