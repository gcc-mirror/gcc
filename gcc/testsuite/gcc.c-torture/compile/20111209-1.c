/* { dg-do compile { target s390x-*-* *-*-*vms* } } */

typedef char* char_ptr32 __attribute__ ((mode(SI)));

char_ptr32 getenv (const char *name);
unsigned long strlen (const char *str);

void
__gnat_getenv (char *name, int *len, char **value)
{
  *value = getenv (name);
  *len = strlen (*value);
}
