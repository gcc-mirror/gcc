
// expands a macro (inside #if conditional) inside forced header.
// modelled on glibc's stdc_predef.h

// some builtin macro
# if __GNUC__ > 0
# endif
