/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-fPIE -Wwrite-strings" } */

char *strcpy (char *dest, const char *src);

static __thread char buffer[25];
const char * error_message (void)
{
oops:
    strcpy (buffer, "Unknown code ");
    return 0;
}
