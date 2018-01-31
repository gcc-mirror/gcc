/* { dg-do compile } */

/* Check 'uncached' type attribute.  */

typedef volatile unsigned int RwReg  __attribute__ ((uncached));

typedef struct {
  RwReg UART_THR;
  int SIDE_DISH;
} UART;

void uart_putc(UART *port, char c)
{
    port->UART_THR = c;
    port->SIDE_DISH = c;
}

/* { dg-final { scan-assembler-times "st\.di" 1 } } */
