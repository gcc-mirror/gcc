#ifdef __mips
#define SDATA_SECTION __attribute__((__section__(".sdata")))
#else
#define SDATA_SECTION
#endif

extern void abort (void);
