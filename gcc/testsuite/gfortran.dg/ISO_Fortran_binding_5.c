/* Test fix for PR89385.  */

/* Contributed by Reinhold Bader  <Bader@lrz.de>  */

#include <stdio.h>
#include <math.h>
#include "../../../libgfortran/ISO_Fortran_binding.h"

typedef struct {
  int i;
  float r[2];
} cstruct;


void Psub(CFI_cdesc_t *this, CFI_cdesc_t *that, int *ierr) {
    int status = 0;
    cstruct *cu;
    float *ct;
    CFI_dim_t *dim;
    if (this->elem_len != sizeof(float)) {
	printf("FAIL: this->elem_len %i\n",(int) this->elem_len);
	status++;
    }
    if (this->type != CFI_type_float) {
	printf("FAIL: this->type\n");
	status++;
    }
    if (this->rank != 2) {
	printf("FAIL: this->rank %i\n",this->rank);
	status++;
    }
    if (this->attribute != CFI_attribute_allocatable) {
	printf("FAIL: this->attribute\n");
	status++;
    }
    dim = this->dim;
    if (dim[0].lower_bound != 3 || dim[0].extent != 4)  {
	printf("FAIL: dim[0] %d %d\n", dim[0].lower_bound, dim[0].extent);
	status++;
    }
    if (dim[1].lower_bound != 1 || dim[1].extent != 5)  {
	printf("FAIL: dim[1] %d %d\n", dim[1].lower_bound, dim[1].extent);
	status++;
    }

    if (that->elem_len != sizeof(cstruct)) {
	printf("FAIL: that->elem_len\n");
	status++;
    }
    if (that->type != CFI_type_struct) {
	printf("FAIL: that->type %d %d\n", that->type, CFI_type_struct);
	status++;
    }
     if (that->rank != 1) {
	printf("FAIL: that->rank\n");
	status++;
    }
    if (that->attribute != CFI_attribute_allocatable) {
	printf("FAIL: that->attribute\n");
	status++;
    }
    dim = that->dim;
    if (dim[0].lower_bound != 1 || dim[0].extent != 1)  {
	printf("FAIL: dim[0] %d %d\n" , dim[0].lower_bound, dim[0].extent);
	status++;
    }
    cu = (cstruct *) ((CFI_cdesc_t *) that)->base_addr;
    if (cu->i != 4 || fabs(cu->r[1] -  2.2) > 1.0e-6) {
	printf("FAIL: value of that %i %f %f\n",cu->i,cu->r[1],cu->r[2]);
	status++;
    }

    ct = (float *) ((CFI_cdesc_t *) this)->base_addr;
    if ( fabs(ct[5] +  2.0) > 1.0e-6) {
	printf("FAIL: value of this %f\n",ct[5]);
	status++;
    }


    *ierr = status;

}

