/* { dg-do compile } */
/* { dg-options "" } */

/* Make sure that section names that contain characters not in the set
   [0-9a-zA-Z_] get quoted for the assembler.  */

__attribute__((section ("uretprobe//proc/self/exe:trigger_func2")))
void
foo ()
{
}

__attribute__((section ("trigger_func3")))
void
bar ()
{
}

/* { dg-final { scan-assembler {\.section\t"uretprobe//proc/self/exe:trigger_func2"} } } */
/* { dg-final { scan-assembler {\.section\ttrigger_func3} } } */
