/* { dg-do compile } */
/* { dg-options "-fPIE -Wwrite-strings" } */
/* { dg-require-effective-target pie } */

char *strcpy (char *dest, const char *src);

static __thread char buffer[25];
const char * error_message (void)
{
oops:
    strcpy (buffer, "Unknown code ");
    return 0;
}
