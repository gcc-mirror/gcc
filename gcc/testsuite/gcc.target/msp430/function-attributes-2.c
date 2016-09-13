void __attribute__((weak, interrupt(10)))
weak_interrupt_number (void) {
} /* { dg-error "argument to interrupt attribute is unsupported for weak functions" } */
