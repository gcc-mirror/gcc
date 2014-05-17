int func () __attribute__ ((break_handler));
volatile int intr_occurred;

int func ()
{

  /* { dg-final { scan-assembler "rtbd\tr(\[0-9]\|\[1-2]\[0-9]\|3\[0-1]),8" } } */
    intr_occurred += 1;
}
int main()
{
    /* { dg-final { scan-assembler "brki\tr16" } } */
    func();
    return 0;
}
