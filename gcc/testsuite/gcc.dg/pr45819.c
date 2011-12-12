/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -w" } */

struct ehci_regs {
    char x;
    unsigned int port_status[0];
} __attribute__ ((packed));

struct ehci_hcd {
    struct ehci_regs *regs;
};

int ehci_hub_control (struct ehci_hcd *ehci, int wIndex)
{
  unsigned int *status_reg = &ehci->regs->port_status[wIndex];
  return *(volatile unsigned int *)status_reg;
}

/* { dg-final { scan-tree-dump "={v}" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
