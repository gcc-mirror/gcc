/* { dg-do assemble } */

char insert (long a)
{
    return __builtin_avr_insert_bits (15.3f+a, 0, 0); /* { dg-error "expects a compile time" } */
}

void delay (long a)
{
    __builtin_avr_delay_cycles (a); /* { dg-error "expects a compile time" } */
}
