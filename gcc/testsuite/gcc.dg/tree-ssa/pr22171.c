/* { dg-options "-O2" } */

static unsigned int old_serial_port_irq[10];

static struct uart_8250_port {
  volatile unsigned int slock;
  unsigned int irq;
} serial8250_ports[10];

static __inline__ __attribute__((always_inline)) int irq_canonicalize(int irq)
{
  return ((irq == 2) ? 9 : irq);
}

void serial8250_isa_init_ports(void)
{
  struct uart_8250_port *up;
  int i;

  for (i = 0, up = serial8250_ports; i < 10; i++, up++)
    up->irq = irq_canonicalize(old_serial_port_irq[i]);
}

int main(void)
{
  serial8250_isa_init_ports();
  return 0;
}
