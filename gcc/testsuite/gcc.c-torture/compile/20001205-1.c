/* { dg-do assemble } */

static inline unsigned long rdfpcr(void)
{
        unsigned long tmp, ret;
        __asm__ ("" : "=r"(tmp), "=r"(ret));
        return ret;
}

static inline unsigned long
swcr_update_status(unsigned long swcr, unsigned long fpcr)
{
	swcr &= ~0x7e0000ul;
        swcr |= (fpcr >> 3) & 0x7e0000ul;
        return swcr;
}

unsigned long osf_getsysinfo(unsigned long flags)
{
        unsigned long w;
	w = swcr_update_status(flags, rdfpcr());
	return w;
}
