/* { dg-do run } */
/* { dg-options "-Os -save-temps" } */

__attribute__((address(1234)))
int g_1234;

__attribute__((weak, address(4321)))
int w_4321;

__attribute__((address(5678)))
static int l_5678;

__attribute__((io_low(__AVR_SFR_OFFSET__ + 3)))
volatile unsigned char g_low;

__attribute__((weak, io_low(__AVR_SFR_OFFSET__ + 2)))
volatile unsigned char w_low;

__attribute__((io_low(__AVR_SFR_OFFSET__ + 1)))
static volatile unsigned char l_low;

__attribute__((io(__AVR_SFR_OFFSET__ + 35)))
volatile unsigned char g_io;

__attribute__((weak, io(__AVR_SFR_OFFSET__ + 34)))
volatile unsigned char w_io;

__attribute__((io(__AVR_SFR_OFFSET__ + 33)))
static volatile unsigned char l_io;

#define CMP(SYM, VAL)				\
  do {						\
    unsigned x;					\
    __asm ("" : "=d" (x) : "0" (& SYM));	\
    if (x != VAL)				\
      __builtin_abort();			\
  } while(0)


int main (void)
{
  CMP (g_1234, 1234);
  CMP (w_4321, 4321);
  CMP (l_5678, 5678);

  CMP (g_low, __AVR_SFR_OFFSET__ + 3);
  CMP (w_low, __AVR_SFR_OFFSET__ + 2);
  CMP (l_low, __AVR_SFR_OFFSET__ + 1);

  CMP (g_io, __AVR_SFR_OFFSET__ + 35);
  CMP (w_io, __AVR_SFR_OFFSET__ + 34);
  CMP (l_io, __AVR_SFR_OFFSET__ + 33);

  l_low = l_io;
  g_low = g_io;
  w_low = w_io;
  l_low |= 1;
  g_low |= 2;
  w_low |= 4;

  return 0;
}

/* { dg-final { scan-assembler "g_1234 = 1234" } } */
/* { dg-final { scan-assembler "w_4321 = 4321" } } */
/* { dg-final { scan-assembler "l_5678 = 5678" } } */

/* { dg-final { scan-assembler "\\.globl	g_1234" } } */
/* { dg-final { scan-assembler "\\.globl	g_low" } } */
/* { dg-final { scan-assembler "\\.globl	g_io" } } */

/* { dg-final { scan-assembler "\\.weak	w_4321" } } */
/* { dg-final { scan-assembler "\\.weak	w_low" } } */
/* { dg-final { scan-assembler "\\.weak	w_io" } } */
