/* Test the fix for PR89841.  */

/* Contributed by Reinhold Bader  <Bader@lrz.de> */

#include "../../../libgfortran/ISO_Fortran_binding.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct
  {
    int i;
    float r[2];
  } cstruct;


int Psuba(CFI_cdesc_t *this, CFI_cdesc_t *that, int Dcase) {
    int status = 0;
    cstruct *cu;
    float *ct;
    CFI_dim_t *dim;
    if (this->elem_len != sizeof(float))
      {
	printf("FAIL: Dcase %i - this->elem_len %i\n",Dcase, (int) this->elem_len);
	status++;
      }
    if (this->type != CFI_type_float)
      {
	printf("FAIL: Dcase %i - this->type\n", Dcase);
	status++;
      }
    if (this->rank != 2)
      {
	printf("FAIL: Dcase %i - this->rank %i\n",Dcase,this->rank);
	status++;
      }
    if (this->attribute != CFI_attribute_other)
      {
	printf("FAIL: Dcase %i - this->attribute\n", Dcase);
	status++;
      }

    dim = this->dim;
    if (dim[0].lower_bound != 0 || dim[0].extent != 3) 
      {
	printf("FAIL: Dcase %i - dim[0] %i %i %i\n",Dcase, (int) dim[0].lower_bound,
	      (int)dim[0].extent,(int)dim[0].sm);
	status++;
      }
    if (dim[1].lower_bound != 0 || dim[1].extent != 7)
      {
	printf("FAIL: Dcase %i - dim[1] %i %i %i\n",Dcase,(int) dim[1].lower_bound,
	      (int) dim[1].extent,(int) dim[1].sm);
	status++;
      }

    if (that->elem_len != sizeof(cstruct))
      {
	printf("FAIL: Dcase %i - that->elem_len\n", Dcase);
	status++;
      }
    if (that->type != CFI_type_struct)
      {
	printf("FAIL: Dcase %i - that->type\n",Dcase);
	status++;
      }
     if (that->rank != 1)
      {
	printf("FAIL: Dcase %i - that->rank\n", Dcase);
	status++;
      }
    if (that->attribute != CFI_attribute_other)
      {
	printf("FAIL: Dcase %i - that->attribute %i\n",Dcase,that->attribute);
	status++;
      }

    dim = that->dim;
    if (dim[0].lower_bound != 0 || dim[0].extent != 1) 
      {
	printf("FAIL: Dcase %i - dim[0] %i %i\n",Dcase,(int)dim[0].lower_bound,dim[0].extent);
	status++;
      }

    cu = (cstruct *) ((CFI_cdesc_t *) that)->base_addr;
    if (cu->i != 4 || fabs(cu->r[1] -  2.2) > 1.0e-6)
      {
	printf("FAIL: Dcase %i - value of that %i %f %f\n",Dcase,cu->i,cu->r[0],cu->r[1]);
	status++;
      } 

    ct = (float *) ((CFI_cdesc_t *) this)->base_addr;
    if ( fabs(ct[5] +  2.0) > 1.0e-6)
      {
	printf("FAIL: Dcase %i - value of this %f\n",Dcase,ct[5]);
	status++;
      }
 
    return status;
}


