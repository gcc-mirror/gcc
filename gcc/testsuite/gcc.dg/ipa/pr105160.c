/* { dg-do compile } */
/* { dg-options "-O -fdump-ipa-modref" } */
#define sysreg_read(regname)		\
({					\
	unsigned long __sr_val;		\
	asm volatile("");		\
					\
	__sr_val;			\
})

#define sysreg_write(regname, __sw_val)	\
do {					\
	asm volatile("");			\
} while (0)

#define isb()				\
do {					\
	asm volatile(			\
	"isb"				\
	:				\
	:				\
	: "memory");			\
} while (0)

static unsigned long sctlr_read(void)
{
	return sysreg_read(sctlr_el1);
}

static void sctlr_write(unsigned long val)
{
	sysreg_write(sctlr_el1, val);
}

static void sctlr_rmw(void)
{
	unsigned long val;

	val = sctlr_read();
	val |= 1UL << 7;
	sctlr_write(val);
}

void sctlr_read_multiple(void)
{
	sctlr_read();
	sctlr_read();
	sctlr_read();
	sctlr_read();
}

void sctlr_write_multiple(void)
{
	sctlr_write(0);
	sctlr_write(0);
	sctlr_write(0);
	sctlr_write(0);
	sctlr_write(0);
}

void sctlr_rmw_multiple(void)
{
	sctlr_rmw();
	sctlr_rmw();
	sctlr_rmw();
	sctlr_rmw();
}

void function(void)
{
	sctlr_read_multiple();
	sctlr_write_multiple();
	sctlr_rmw_multiple();

	isb();
}
/* { dg-final { scan-ipa-dump-not "Function found to be const" "modref"  } } */
