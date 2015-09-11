/* { dg-do compile } */
/* { dg-options "-fstrict-volatile-bitfields -fdump-rtl-final" } */

#define PERIPH (*(volatile struct system_periph *)0x81234)

struct system_periph {
  union {
    unsigned short WORD;
    struct {
      unsigned short a:1;
      unsigned short b:1;
      unsigned short  :5;
      unsigned short c:1;
      unsigned short  :8;
    } BIT;
  } ALL;
} __attribute__((aligned(2)));

void
foo()
{
  while (1)
    {
      PERIPH.ALL.BIT.a = 1;
    }
}
/* { dg-final { scan-rtl-dump-times "mem/v(/.)*:HI" 4 "final" } } */
