/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef long (*EFI_PCI_IO_PROTOCOL_CONFIG)();
typedef struct {
  EFI_PCI_IO_PROTOCOL_CONFIG Read;
} EFI_PCI_IO_PROTOCOL_CONFIG_ACCESS;
typedef struct {
  EFI_PCI_IO_PROTOCOL_CONFIG_ACCESS Pci;
} EFI_PCI_IO_PROTOCOL;
int init_regs_0;
static void __attribute__((constructor)) init(EFI_PCI_IO_PROTOCOL *pci_io) {
  if (init_regs_0)
    pci_io->Pci.Read();
}
