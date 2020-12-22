
// expands a macro (inside #if conditional) inside forced header.
// modelled on glibc's stdc_predef.h

#define GCC_IEC_559 1

# if GCC_IEC_559 > 0
#  define STDC_IEC_559__		1
# endif

