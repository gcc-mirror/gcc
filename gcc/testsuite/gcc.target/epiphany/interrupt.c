void __attribute__((interrupt("dma0")))
f (void)
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
