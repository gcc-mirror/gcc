/* { dg-do compile } */
/* { dg-options "-mint-register=3 -msave-acc-in-interrupts" } */

/* Verify that the RX specific function attributes work.  */

void fast_interrupt (void) __attribute__((__fast_interrupt__));
void interrupt (void) __attribute__((__interrupt__));
int naked (int) __attribute__((__naked__));

int flag = 0;

/* Fast interrupt handler.  Only uses registers marked as fixed
   by the -fixed-xxx gcc command line option.  Returns via RTFI.  */

void
fast_interrupt (void)
{
  flag = 1;
}

/* Interrupt handler.  Must preserve any register it uses, even
   call clobbered ones.  Returns via RTE.  */

void
interrupt (void)
{
  switch (flag)
    {
    case 0:
      flag = -1;
      break;
    case 1:
    case 2:
    case 4:
      flag = flag - 2;
      break;
    case 5:
    case 7:
    case 6:
      flag ^= 3;
      break;
    default:
      naked (flag * 2);
      break;
    }
}

/* Naked function.  The programmer must supply the function's
   prologue and epilogue instructions.  */

int
naked (int arg)
{
  flag = arg;
}

/* { dg-final { scan-assembler "rtfi" } } */
/* { dg-final { scan-assembler "rte" } } */
