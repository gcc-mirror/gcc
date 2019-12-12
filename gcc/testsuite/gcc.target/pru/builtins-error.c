/* { dg-do assemble } */

void delay (long a)
{
    __delay_cycles (a); /* { dg-error "'__delay_cycles' only takes constant arguments" } */
}
