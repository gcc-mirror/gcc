void __attribute__((weak, interrupt))
weak_interrupt (void) {
}

void __attribute__((interrupt(11)))
interrupt_number (void) {
}

/* { dg-final { scan-assembler-times "__interrupt_vector_" 1 } } */
