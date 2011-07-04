/* { dg-do run } */

#include "../exit-abort.h"
#include "../progmem.h"

const char strA[] PROGMEM = "@A";
const char strc PROGMEM = 'c';

unsigned int volatile s = 2;

int main()
{
    char c;
    
    c = pgm_read_char (&strA[s-1]);
    if (c != 'A')
        abort();

    c = pgm_read_char (&PSTR ("@@B")[s]);
    if (c != 'B')
        abort();

    c = pgm_read_char (&strc);
    if (c != 'c')
        abort();

    exit (0);

    return 0;
}
