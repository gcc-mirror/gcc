/* { dg-do run } */

typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;

typedef uint32_t T;

#define NI __attribute__((noipa))


#ifndef __AVR_TINY__
#define read_u32(X)                                                     \
    (__extension__(                                                     \
        {                                                               \
            uint16_t __addr16 = (uint16_t)(X);                          \
            uint32_t __result;                                          \
            __asm__ __volatile__ ("lpm %A0, Z+" "\n\t"                  \
                                  "lpm %B0, Z+" "\n\t"                  \
                                  "lpm %C0, Z+" "\n\t"                  \
                                  "lpm %D0, Z" "\n\t"                   \
                                  : "=r" (__result), "+z" (__addr16));  \
            __result;                                                   \
        }))
#else
NI uint32_t read_u32 (const uint32_t *p)
{
	return *p;
}
#endif

static const __attribute((progmem)) T xyz_prog[] = { 123, 123, 123 };
T xyz[] = { 123, 123, 123 };
volatile int x = 0;

NI void prf (T f)
{
    if (f != 123)
        __builtin_abort();
}

NI void func_progmem()
{
    prf (read_u32 (&xyz_prog[0]));
}

NI void func_ram()
{
    prf (xyz[x]);
}

int main (void)
{
    func_progmem();
    func_ram();

	return 0;
}
