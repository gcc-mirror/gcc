/* Configuration for an i386 running Mach as the target machine.  */

#define TARGET_VERSION fprintf (stderr, " (80386, Mach)"); 

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -DMACH -Asystem=unix -Asystem=mach"

/* Specify extra dir to search for include files.  */
#define SYSTEM_INCLUDE_DIR "/usr/mach/include"

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0
