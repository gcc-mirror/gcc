/* { dg-options "-g" } */

void __attribute__((interrupt))
universal_handler (void)
{
}

void __attribute__((interrupt("dma0","Vss","dma1")))
g (void)
{ /* { dg-warning "is not \"reset\"" } */
}

void __attribute__((interrupt("dma0","dma1","timer1","reset"),
		    forwarder_section("test")))
misc_handler (void)
{
}

void __attribute__((interrupt(dma0,42)))
h (void)
{ /* { dg-warning "is not a string constant" } */
}

/* { dg-final { scan-assembler-times "b\[ \t\]*_misc_handler" 4 } } */
