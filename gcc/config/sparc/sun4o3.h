#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dsparc -Dsun -Dunix -Asystem=unix -Asystem=bsd"

/* Override the name of the mcount profiling function.  */

#undef MCOUNT_FUNCTION
#define MCOUNT_FUNCTION "*.mcount"

/* LINK_SPEC is needed only for SunOS 4.  */

#undef LINK_SPEC
