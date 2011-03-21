#include "check.h"


#define ASMNAME(cname)  ASMNAME2 (__USER_LABEL_PREFIX__, cname)
#define ASMNAME2(prefix, cname) STRING (prefix) cname
#define STRING(x)    #x

#ifdef  __cplusplus
extern "C" void abort (void);
#else
extern void abort (void);
#endif

extern void foo(void);

#define INIT_EDI 1
#define INIT_ESI 2
#define INIT_EBX 3

/* Set DI/SI/BX to wrong value
   Use following template so that RA will save/restore callee
   save registers in prologue/epilogue */
#define ALTER_REGS() \
  { \
        int dummy;      \
        __asm__  __volatile__ (\
        "movl %1, %0" : "=D" (dummy) : "i" (-INIT_EDI)\
        );\
        __asm__  __volatile__ (\
        "movl %1, %0" : "=S" (dummy) : "i" (-INIT_ESI)\
        );\
        __asm__  __volatile__ (\
        "movl %1, %0" : "=b" (dummy) : "i" (-INIT_EBX)\
        );\
  }

#if defined __PIC__ || defined __USING_SJLJ_EXCEPTIONS__
int
main ()
{
  return 0;
}
#else
void __attribute__ ((noinline))
copy (char *p, int size)
{
  __builtin_strncpy (p, "good", size);
}

int g_edi __attribute__((externally_visible)) =INIT_EDI;
int g_esi __attribute__((externally_visible)) =INIT_ESI;
int g_ebx __attribute__((externally_visible)) = INIT_EBX; 
int g_ebp __attribute__((externally_visible));
int g_esp __attribute__((externally_visible));
int g_ebp_save __attribute__((externally_visible));
int g_esp_save __attribute__((externally_visible));
int n_error;

int
main()
{
        int dummy;
	// Init registers to correct value.
        // Use following template so that RA will save/restore callee
	// save registers in prologue/epilogue
	__asm__  __volatile__ (
	"movl %1, %0"
	: "=D" (dummy)
	: "i" (INIT_EDI)
	);
	__asm__  __volatile__ (
	"movl %1, %0"
	: "=S" (dummy)
	: "i" (INIT_ESI)
	);
	__asm__  __volatile__ (
	"movl %1, %0"
	: "=b" (dummy)
	: "i" (INIT_EBX)
	);
	__asm__ __volatile__ (
	"movl %ebp," ASMNAME("g_ebp_save")"\n\t"
	"movl %esp," ASMNAME("g_esp_save")"\n\t"
	);
	try {
		foo();
	}
	catch (...)
	{
	}

	// Get DI/SI/BX register value after exception caught
	__asm__ __volatile__ (
	"movl %edi," ASMNAME("g_edi")"\n\t"
	"movl %esi," ASMNAME("g_esi")"\n\t"
	"movl %ebx," ASMNAME("g_ebx")"\n\t"
	"movl %ebp," ASMNAME("g_ebp")"\n\t"
	"movl %esp," ASMNAME("g_esp")"\n\t"
	);

	// Check if DI/SI/BX register value are the same as before calling
        // foo.
	if (g_edi != INIT_EDI)
	{
		n_error++;
#ifdef DEBUG
		printf("edi=%d, correct value:%d\n", g_edi, INIT_EDI);
#endif
	}
	if (g_esi != INIT_ESI)
	{
		n_error++;
#ifdef DEBUG
		printf("esi=%d, correct value:%d\n", g_esi, INIT_ESI);
#endif
	}
	if (g_ebx != INIT_EBX)
	{
		n_error++;
#ifdef DEBUG
		printf("ebx=%d, correct value:%d\n", g_ebx, INIT_EBX);
#endif
	}
	if (g_ebp != g_ebp_save)
	{
		n_error++;
#ifdef DEBUG
		printf("ebp=0x%x, correct value:0x%x\n", g_ebp, g_ebp_save);
#endif
	}
	if (g_esp != g_esp_save)
	{
		n_error++;
#ifdef DEBUG
		printf("esp=0x%x, correct value:0x%x\n", g_esp, g_esp_save);
#endif
	}
	if (n_error !=0)
		abort();
	return 0;
}
#endif
