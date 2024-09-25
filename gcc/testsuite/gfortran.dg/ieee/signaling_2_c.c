int isnansf (float x)       { return __builtin_issignaling (x) ? 1 : 0; }
int isnans  (double x)      { return __builtin_issignaling (x) ? 1 : 0; }
int isnansl (long double x) { return __builtin_issignaling (x) ? 1 : 0; }

