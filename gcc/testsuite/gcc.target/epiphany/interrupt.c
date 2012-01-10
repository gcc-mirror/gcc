/* { dg-options "-g" } */

void __attribute__((interrupt("dma0")))
dma0_handler (void)
{
}

void __attribute__((interrupt("Vss")))
g (void)
{ /* { dg-warning "is not \"reset\"" } */
}

void __attribute__((interrupt(42)))
h (void)
{ /* { dg-warning "is not a string constant" } */
}

/* { dg-final { scan-assembler-times "b\[ \t\]*_dma0_handler" 1 } } */
