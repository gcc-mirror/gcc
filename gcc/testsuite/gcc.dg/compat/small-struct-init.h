/* Function definitions that are used by multiple tests.  */

void initSc (Sc *p, int i) { p->c = (char)i; }
void initSs (Ss *p, int i) { p->s = i; }
void initSi (Si *p, int i) { p->i = i; }
void initSsc (Ssc *p, int i) { p->s = i; p->c = (char)i+1; }
void initScs (Scs *p, int i) { p->c = (char)i; p->s = i+1; }
void initSsi (Ssi *p, int i) { p->s = i; p->i = i+1; }
void initSis (Sis *p, int i) { p->i = i; p->s = i+1; }
void initSic (Sic *p, int i) { p->i = i; p->c = (char)i+1; }
void initSci (Sci *p, int i) { p->c = (char)i; p->i = i+1; }
void initScsi (Scsi *p, int i) { p->c = (char)i; p->s = i+1; p->i = i+2; }
void initScis (Scis *p, int i) { p->c = (char)i; p->i = i+1; p->s = i+2; }
void initSsci (Ssci *p, int i) { p->s = i; p->c = (char)i+1; p->i = i+2; }
void initSsic (Ssic *p, int i) { p->s = i; p->i = i+1; p->c = (char)i+2; }
void initSisc (Sisc *p, int i) { p->i = i; p->s = i+1; p->c = (char)i+2; }
void initSics (Sics *p, int i) { p->i = i; p->c = (char)i+1; p->s = i+2; }
