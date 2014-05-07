int *p;

void __attribute__((interrupt))
isr (int signum) /* { dg-error "interrupt handlers cannot have arguments"  } */
{
  *p = 1;
  return;
}

