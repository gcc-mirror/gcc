#define P(a, b) P1(a,b)
#define P1(a,b) a##b

#define FLT_MIN_EXP (-125)
#define DBL_MIN_EXP (-1021)

#define MIN_EXP P(FLT,_MIN_EXP)

#define FLT FLT
int f1 = MIN_EXP;

#undef FLT
#define FLT DBL
int f2 = MIN_EXP;
