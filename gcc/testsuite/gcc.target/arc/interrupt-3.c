void __attribute__ ((interrupt))
handler0 (void)
{ /* { dg-error "wrong number of arguments specified" } */
}

void __attribute__ ((interrupt("you load too")))
handler1 (void)
{ /* { dg-warning "is not \"ilink1\" or \"ilink2\"" } */
}

void __attribute__ ((interrupt(42)))
hander2 (void)
{ /* { dg-warning "is not a string constant" } */
}
