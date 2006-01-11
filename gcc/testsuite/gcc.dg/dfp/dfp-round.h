/* This is very temporary; right now it gets functions in libgcc that
   pass on the rounding mode to decNumber, but later it can be replaced
   with Official Stuff.  */

#define FE_DEC_DOWNWARD 0
#define FE_DEC_TONEAREST 1
#define FE_DEC_TONEARESTFROMZERO 2
#define FE_DEC_TOWARDZERO 3
#define FE_DEC_UPWARD 4

extern void __dfp_set_round (int);
#define DFP_SETROUND(M) __dfp_set_round(M)
extern int __dfp_get_round (void);
#define DFP_GETROUND __dfp_get_round()
