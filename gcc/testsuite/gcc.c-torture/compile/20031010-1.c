/* This crashed the ARM backend with -mcpu=iwmmxt -O because an insn
   required a split which was not available for the iwmmxt.  */
inline int *f1(int* a, int* b) { if (*b < *a) return b; return a; }
int f2(char *d, char *e, int f) { int g = e - d; return *f1(&f, &g); }
