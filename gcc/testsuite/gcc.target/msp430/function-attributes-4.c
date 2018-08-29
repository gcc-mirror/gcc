/* { dg-do compile } */
/* Check that the foo interrupt vectors aren't actually removed.  */
/* { dg-final { scan-assembler-times "__interrupt_vector_foo" 2 } } */

/* Check that warnings are emitted when attributes are used incorrectly and
   that attributes are interpreted correctly whether leading and trailing
   underscores are used or not.  */

void __attribute__((__naked__,__reentrant__))
fn1(void)
{ /* { dg-warning "naked functions cannot be reentrant" } */
}

void __attribute__((naked,reentrant))
fn2(void)
{ /* { dg-warning "naked functions cannot be reentrant" } */
}

void __attribute__((__reentrant__,__naked__))
fn3(void)
{ /* { dg-warning "reentrant functions cannot be naked" } */
}

void __attribute__((reentrant,naked))
fn4(void)
{ /* { dg-warning "reentrant functions cannot be naked" } */
}

void __attribute__((__critical__,__reentrant__))
fn5(void)
{ /* { dg-warning "critical functions cannot be reentrant" } */
}

void __attribute__((critical,reentrant))
fn6(void)
{ /* { dg-warning "critical functions cannot be reentrant" } */
}

void __attribute__((__reentrant__,__critical__))
fn7(void)
{ /* { dg-warning "reentrant functions cannot be critical" } */
}

void __attribute__((reentrant,critical))
fn8(void)
{ /* { dg-warning "reentrant functions cannot be critical" } */
}

void __attribute__((__critical__,__naked__))
fn9(void)
{ /* { dg-warning "critical functions cannot be naked" } */
}

void __attribute__((critical,naked))
fn10(void)
{ /* { dg-warning "critical functions cannot be naked" } */
}

void __attribute__((__naked__,__critical__))
fn11(void)
{ /* { dg-warning "naked functions cannot be critical" } */
}

void __attribute__((naked,critical))
fn12(void)
{ /* { dg-warning "naked functions cannot be critical" } */
}

int __attribute__((interrupt))
isr1 (void)
{ /* { dg-warning "interrupt handlers must be void" } */
}

int __attribute__((__interrupt__))
isr2 (void)
{ /* { dg-warning "interrupt handlers must be void" } */
}

void __attribute__((interrupt("foo1")))
isr3 (void)
{ /* { dg-warning "unrecognized interrupt vector argument" } */
}

void __attribute__((__interrupt__("foo2")))
isr4 (void)
{ /* { dg-warning "unrecognized.*interrupt vector argument" } */
}

void __attribute__((interrupt(65)))
isr5 (void)
{ /* { dg-warning "numeric argument of 'interrupt' attribute must be in range 0..63" } */
}

void __attribute__((__interrupt__(100)))
isr6 (void)
{ /* { dg-warning "numeric argument of 'interrupt' attribute must be in range 0..63" } */
}

void __attribute__((interrupt(0.5)))
isr7 (void)
{ /* { dg-warning "argument of 'interrupt' attribute is not a string constant or number" } */
  volatile int __attribute__((__naked__))
    a; /* { dg-warning "'naked' attribute only applies to functions" } */
}

void __attribute__((__interrupt__(1.5)))
isr8 (void)
{ /* { dg-warning "argument of 'interrupt' attribute is not a string constant or number" } */
  volatile int __attribute__((naked))
    a; /* { dg-warning "'naked' attribute only applies to functions" } */
}
