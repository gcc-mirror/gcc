/* Test the special handling of .pru_irq_map section.  */

/* { dg-do compile } */

int my_int_map __attribute__((section(".pru_irq_map")));

/* Section must not have the allocated flag.  */
/* { dg-final { scan-assembler "\.section\[ \t\]+.pru_irq_map,\[ \]*\"\",[ ]*@progbits" } } */
