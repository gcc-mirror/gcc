/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\[us\]xtw\t" } } */
/* { dg-final { scan-assembler-not "\[us\]bfiz\t" } } */
/* { dg-final { scan-assembler-not "lsl\t" } } */

int
load_scaled_sxtw (int *arr, int i)
{
  return arr[arr[i]];
}

unsigned int
load_scaled_uxtw (unsigned int *arr, unsigned int i)
{
  return arr[arr[i]];
}

void
store_scaled_sxtw (int *arr, int i)
{
  arr[arr[i]] = 0;
}

void
store_scaled_uxtw (unsigned int *arr, unsigned int i)
{
  arr[arr[i]] = 0;
}

int
load_unscaled_sxtw (signed char *arr, int i)
{
  return arr[arr[i]];
}

unsigned int
load_unscaled_uxtw (unsigned char *arr, unsigned int i)
{
  return arr[arr[i]];
}

void
store_unscaled_sxtw (signed char *arr, int i)
{
  arr[arr[i]] = 0;
}

void
store_unscaled_uxtw (unsigned char *arr, unsigned int i)
{
  arr[arr[i]] = 0;
}



int
load_scaled_tmp_sxtw (int *arr, int i)
{
  int j = arr[i];
  return arr[j];
}

unsigned int
load_scaled_tmp_uxtw (unsigned int *arr, unsigned int i)
{
  unsigned int j = arr[i];
  return arr[j];
}

void
store_scaled_tmp_sxtw (int *arr, int i)
{
  int j = arr[i];
  arr[j] = 0;
}

void
store_scaled_tmp_uxtw (unsigned int *arr, unsigned int i)
{
  unsigned int j = arr[i];
  arr[j] = 0;
}

int
load_unscaled_tmp_sxtw (signed char *arr, int i)
{
  signed char j = arr[i];
  return arr[j];
}

unsigned int
load_unscaled_tmp_uxtw (unsigned char *arr, unsigned int i)
{
  unsigned char j = arr[i];
  return arr[j];
}

void
store_unscaled_tmp_sxtw (signed char *arr, int i)
{
  signed char j = arr[i];
  arr[j] = 0;
}

void
store_unscaled_tmp_uxtw (unsigned char *arr, unsigned int i)
{
  unsigned char j = arr[i];
  arr[j] = 0;
}
