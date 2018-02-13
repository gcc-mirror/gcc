/* { dg-do compile } */

void __attribute__ ((interrupt (5))) interrupt_5_handler ();

void interrupt_5_handler ()
{
}

void __attribute__ ((vector (4))) interrupt_4_handler ();

void interrupt_4_handler ()
{
}

void __attribute__ ((interrupt)) interrupt_handler ();

void interrupt_handler ()
{
}

/* { dg-final { scan-assembler "tableentry" } } */
