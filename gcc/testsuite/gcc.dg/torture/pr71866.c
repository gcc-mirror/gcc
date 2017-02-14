/* { dg-do compile } */
/* { dg-additional-options "-ftree-pre -fcode-hoisting" } */

__extension__ typedef __UINTPTR_TYPE__ uintptr_t;

typedef unsigned char u8;
extern unsigned long pci_io_base;
u8 in_8 (const volatile void *);
int eeh_enabled ();
void eeh_check_failure ();
static inline 
u8 eeh_readb(const volatile void *addr)
{
  u8 val = in_8(addr);
  if (((val) == (u8)~0 && eeh_enabled()))   eeh_check_failure();
  return val;
}
extern struct ppc_pci_io {
    void (*outb) (u8 val, unsigned long port);
}
ppc_pci_io;
static inline
u8 readb (const volatile void * addr)
{
  return eeh_readb((addr));
}
static inline
u8 inb (unsigned long port)
{
  return readb((volatile void *)(uintptr_t)pci_io_base + port);
}
static inline
void outb (u8 val, unsigned long port)
{
  if (ppc_pci_io.outb != ((void *)0)) ppc_pci_io.outb (val, port);
};
void frob_econtrol(unsigned long base_hi, unsigned char m, unsigned char v)
{
  unsigned char ectr = 0;
  if (m != 0xff)   ectr = inb((base_hi + 0x2));
  outb((ectr & ~m) ^ v, (base_hi + 0x2));
}
