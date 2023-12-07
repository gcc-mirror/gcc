/* Reduced from qemu-7.2.0's hw/intc/omap_intc.c as per
   null-deref-pr108806.c, but with the:
     struct omap_intr_handler_bank_s* bank = NULL;
   converted to:
     struct omap_intr_handler_bank_s* bank;
 */

/* { dg-additional-options "-Wno-analyzer-symbol-too-complex" } */

typedef unsigned char __uint8_t;
typedef unsigned int __uint32_t;
typedef unsigned long int __uint64_t;
typedef __uint8_t uint8_t;
typedef __uint32_t uint32_t;
typedef __uint64_t uint64_t;
typedef uint64_t hwaddr;
typedef struct omap_intr_handler_s omap_intr_handler;

struct omap_intr_handler_bank_s
{
  uint32_t irqs;
  uint32_t inputs;
  uint32_t mask;
  uint32_t fiq;
  uint32_t sens_edge;
  uint32_t swi;
  unsigned char priority[32];
};

struct omap_intr_handler_s
{
  /* [...snip...] */
  unsigned char nbanks;
  /* [...snip...] */
  int sir_intr[2];
  int autoidle;
  uint32_t mask;
  struct omap_intr_handler_bank_s bank[3];
};

uint64_t
omap2_inth_read(struct omap_intr_handler_s* s, int offset)
{
  int bank_no, line_no;
  struct omap_intr_handler_bank_s* bank;

  if ((offset & 0xf80) == 0x80) {
    bank_no = (offset & 0x60) >> 5;
    if (bank_no < s->nbanks) {
      offset &= ~0x60;
      bank = &s->bank[bank_no];
    } else {
      return 0;
    }
  }

  switch (offset) {
    case 0x10:
      return (s->autoidle >> 2) & 1;

    case 0x14:
      return 1;

    case 0x40:
      return s->sir_intr[0];

    case 0x44:
      return s->sir_intr[1];

    case 0x48:
      return (!s->mask) << 2;

    case 0x4c:
      return 0;

    case 0x50:
      return s->autoidle & 3;

    case 0x80:
      return bank->inputs; /* { dg-bogus "use of uninitialized value 'bank'" "PR analyzer/108806" } */

    case 0x84:
      return bank->mask; /* { dg-bogus "use of uninitialized value 'bank'" "PR analyzer/108806" } */

    case 0x88:
    case 0x8c:
      return 0;

    case 0x90:
      return bank->swi; /* { dg-bogus "use of uninitialized value 'bank'" "PR analyzer/108806" } */

    case 0x94:
      return 0;

    case 0x98:
      return bank->irqs & ~bank->mask & ~bank->fiq; /* { dg-bogus "use of uninitialized value 'bank'" "PR analyzer/108806" } */

    case 0x9c:
      return bank->irqs & ~bank->mask & bank->fiq; /* { dg-bogus "use of uninitialized value 'bank'" "PR analyzer/108806" } */

    case 0x100 ... 0x300:
      bank_no = (offset - 0x100) >> 7;
      if (bank_no > s->nbanks)
        break;
      bank = &s->bank[bank_no];
      line_no = (offset & 0x7f) >> 2;
      return (bank->priority[line_no] << 2) | ((bank->fiq >> line_no) & 1);
  }
  return 0;
}
