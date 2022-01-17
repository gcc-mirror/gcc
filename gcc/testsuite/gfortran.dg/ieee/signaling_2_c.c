#define _GNU_SOURCE
#include <math.h>
#include <float.h>

int isnansf (float x)       { return issignaling (x) ? 1 : 0; }
int isnans  (double x)      { return issignaling (x) ? 1 : 0; }
int isnansl (long double x) { return issignaling (x) ? 1 : 0; }

