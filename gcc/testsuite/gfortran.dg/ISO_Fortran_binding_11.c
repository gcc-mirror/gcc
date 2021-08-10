/* Test the fix of PR89846.

Contributed by Reinhold Bader  <Bader@lrz.de>#include <stdio.h> */

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <ISO_Fortran_binding.h>

typedef struct
{
  char n;
  float r[2];
} t1;

typedef struct
{
  long int i;
  t1 t1;
} t2;



void ta0(CFI_cdesc_t *);
void ta1(CFI_cdesc_t *);

void ti(CFI_cdesc_t *this, int flag)
{
  int status;
  size_t dis;
  CFI_CDESC_T(1) that;
  t1 *ans;

  switch (flag)
    {
    case 0:
      dis = offsetof(t2, t1);
      status = CFI_establish((CFI_cdesc_t *) &that, NULL, CFI_attribute_other,
			     CFI_type_struct, sizeof(t1), 1, NULL);
      if (status != CFI_SUCCESS)
	{
	  printf("FAIL 1 establish: nonzero status %i\n",status);
          exit(1);
	}
      status = CFI_select_part((CFI_cdesc_t *) &that, this, dis, 0);
      if (status != CFI_SUCCESS)
	{
	  printf("FAIL C1: nonzero status %i\n",status);
	  exit(1);
	}
     break;

    case 1:
      dis = offsetof(t2, i);
      status = CFI_establish((CFI_cdesc_t *) &that, NULL, CFI_attribute_other,
			     CFI_type_long, 0, 1, NULL);
      if (status != CFI_SUCCESS)
	{
	  printf("FAIL 2 establish: nonzero status %i\n",status);
	  exit(1);
	}
      status = CFI_select_part((CFI_cdesc_t *) &that, this, dis, 0);
      if (status != CFI_SUCCESS)
	{
	  printf("FAIL C2: nonzero status %i\n",status);
	  exit(1);
	}
    }

  if (CFI_is_contiguous((CFI_cdesc_t *) &that))
    {
      printf("FAIL C: contiguity for flag value %i - is %i\n",flag,
	     CFI_is_contiguous((CFI_cdesc_t *) &that));
    }

  if (flag == 0) ta0((CFI_cdesc_t *) &that);
  if (flag == 1) ta1((CFI_cdesc_t *) &that);
}
