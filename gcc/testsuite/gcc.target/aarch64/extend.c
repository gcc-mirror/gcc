/* { dg-do compile } */
/* { dg-options "-O2" } */

int
ldr_uxtw (int *arr, unsigned int i)
{
  /* { dg-final { scan-assembler "ldr\tw\[0-9\]+,.*uxtw #?2]" } } */
  return arr[i];
}

int
ldr_uxtw0 (char *arr, unsigned int i)
{
  /* { dg-final { scan-assembler "ldr\tw\[0-9\]+,.*uxtw]" } } */
  return arr[i];
}

int
ldr_sxtw (int *arr, int i)
{
  /* { dg-final { scan-assembler "ldr\tw\[0-9\]+,.*sxtw #?2]" } } */
  return arr[i];
}

int
ldr_sxtw0 (char *arr, int i)
{
  /* { dg-final { scan-assembler "ldr\tw\[0-9\]+,.*sxtw]" } } */
  return arr[i];
}

unsigned long long
adddi_uxtw (unsigned long long a, unsigned int i)
{
  /* { dg-final { scan-assembler "add\tx\[0-9\]+,.*uxtw #?3" } } */
  return a + ((unsigned long long)i << 3);
}

unsigned long long
adddi_uxtw0 (unsigned long long a, unsigned int i)
{
  /* { dg-final { scan-assembler "add\tx\[0-9\]+,.*uxtw\n" } } */
  return a + i;
}

long long
adddi_sxtw (long long a, int i)
{
  /* { dg-final { scan-assembler "add\tx\[0-9\]+,.*sxtw #?3" } } */
  return a + ((long long)i << 3);
}

long long
adddi_sxtw0 (long long a, int i)
{
  /* { dg-final { scan-assembler "add\tx\[0-9\]+,.*sxtw\n" } } */
  return a + i;
}

unsigned long long
subdi_uxtw (unsigned long long a, unsigned int i)
{
  /* { dg-final { scan-assembler "sub\tx\[0-9\]+,.*uxtw #?3" } } */
  return a - ((unsigned long long)i << 3);
}

unsigned long long
subdi_uxtw0 (unsigned long long a, unsigned int i)
{
  /* { dg-final { scan-assembler "sub\tx\[0-9\]+,.*uxtw\n" } } */
  return a - i;
}

long long
subdi_sxtw (long long a, int i)
{
  /* { dg-final { scan-assembler "sub\tx\[0-9\]+,.*sxtw #?3" } } */
  return a - ((long long)i << 3);
}

long long
subdi_sxtw0 (long long a, int i)
{
  /* { dg-final { scan-assembler "sub\tx\[0-9\]+,.*sxtw\n" } } */
  return a - (long long)i;
}

unsigned long long
subdi_uxth (unsigned long long a, unsigned short i)
{
  /* { dg-final { scan-assembler "sub\tx\[0-9\]+,.*uxth #?1" } } */
  return a - ((unsigned long long)i << 1);
}

unsigned long long
subdi_uxth0 (unsigned long long a, unsigned short i)
{
  /* { dg-final { scan-assembler "sub\tx\[0-9\]+,.*uxth\n" } } */
  return a - i;
}

long long
subdi_sxth (long long a, short i)
{
  /* { dg-final { scan-assembler "sub\tx\[0-9\]+,.*sxth #?1" } } */
  return a - ((long long)i << 1);
}

long long
subdi_sxth0 (long long a, short i)
{
  /* { dg-final { scan-assembler "sub\tx\[0-9\]+,.*sxth\n" } } */
  return a - (long long)i;
}

unsigned int
subsi_uxth (unsigned int a, unsigned short i)
{
  /* { dg-final { scan-assembler "sub\tw\[0-9\]+,.*uxth #?1" } } */
  return a - ((unsigned int)i << 1);
}

unsigned int
subsi_uxth0 (unsigned int a, unsigned short i)
{
  /* { dg-final { scan-assembler "sub\tw\[0-9\]+,.*uxth\n" } } */
  return a - i;
}

int
subsi_sxth (int a, short i)
{
  /* { dg-final { scan-assembler "sub\tw\[0-9\]+,.*sxth #?1" } } */
  return a - ((int)i << 1);
}

int
subsi_sxth0 (int a, short i)
{
  /* { dg-final { scan-assembler "sub\tw\[0-9\]+,.*sxth\n" } } */
  return a - (int)i;
}

unsigned int
addsi_uxth (unsigned int a, unsigned short i)
{
  /* { dg-final { scan-assembler "add\tw\[0-9\]+,.*uxth #?1" } } */
  return a + ((unsigned int)i << 1);
}

unsigned int
addsi_uxth0 (unsigned int a, unsigned short i)
{
  /* { dg-final { scan-assembler "add\tw\[0-9\]+,.*uxth\n" } } */
  return a + i;
}

int
addsi_sxth (int a, short i)
{
  /* { dg-final { scan-assembler "add\tw\[0-9\]+,.*sxth #?1" } } */
  return a + ((int)i << 1);
}

int
addsi_sxth0 (int a, short i)
{
  /* { dg-final { scan-assembler "add\tw\[0-9\]+,.*sxth\n" } } */
  return a + (int)i;
}
