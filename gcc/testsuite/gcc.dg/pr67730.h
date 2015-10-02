#pragma GCC system_header
#define NULL (void *) 0
#define ONEP (void *) 1
#define RETURN return NULL

extern void sysbar (unsigned char *);

unsigned char *
sysfn1 (void *p)
{
   unsigned char *uc = ONEP;
   uc = ONEP;
   sysbar (ONEP);
   return ONEP;
}

extern void sysbar2 (int);

int
sysfn2 (void)
{
  int a = NULL;
  a = NULL;
  sysbar2 (NULL);
  return NULL;
}

int
sysfn3 (void)
{
  RETURN;
}
