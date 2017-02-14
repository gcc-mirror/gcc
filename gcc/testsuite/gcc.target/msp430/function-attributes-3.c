void __attribute__((interrupt("nmi"))) __attribute__((weak))
interrupt_name_weak (void) {
} /* { dg-error "argument to interrupt attribute is unsupported for weak functions" } */
