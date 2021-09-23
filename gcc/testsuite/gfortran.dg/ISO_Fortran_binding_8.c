/* Test the fix for PR89841.  */

/* Contributed by Reinhold Bader  <Bader@lrz.de> */

#include <ISO_Fortran_binding.h>
#include <stdio.h>

float Cxgl[] = { 1.1, 2.3, 5.1, 4.2 };

void globalp(CFI_cdesc_t *this)
{
  int i, status;
  float *pt;
  CFI_index_t lb[] = { 3 };
  CFI_index_t ub[] = { 6 };
  
  if (this->base_addr == NULL)
    {
      status = CFI_allocate(this, lb, ub, 0);
    }
  else
    {
      printf("FAIL C: already allocated.\n");
      return;
    }

  if (status != CFI_SUCCESS)
    {
      printf("FAIL C: status is %i\n",status);
    }

  pt = (float *) this->base_addr;
  for (i=0; i<4; i++)
    {
      pt[i] = Cxgl[i];
    }
}
