/* Configuration for an i860 running Mach as the target machine.  */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i860 Mach3.x)");

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Di860 -DMACH -Asystem(unix) -Asystem(mach) -Acpu(i860) -Amachine(i860)"

/* Specify extra dir to search for include files.  */
#define SYSTEM_INCLUDE_DIR "/usr/mach/include"

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0
