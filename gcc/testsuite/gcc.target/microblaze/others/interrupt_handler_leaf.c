int leaf_func () __attribute__ ((interrupt_handler));
volatile int intr_occurred;

int leaf_func ()
{

  /* { dg-final { scan-assembler "rtid\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),0" } } */
  /* { dg-final { scan-assembler-not "rtsd" } } */    
    intr_occurred += 1;
}
