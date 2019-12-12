/* Test fix for PR89366.  */

/* Contributed by Reinhold Bader  <Bader@lrz.de>  */

#include <stdio.h>
#include <math.h>
#include "../../../libgfortran/ISO_Fortran_binding.h"

#define DEBUG 0

void process_string(CFI_cdesc_t *this, int *ierr) {
  char *cstr;
  cstr = (char *) this->base_addr;
  *ierr = 0;
  if (this->rank != 0) {
    *ierr = 1;
    return;
  }
  if (DEBUG == 1) {
    printf("elem_len member has value %i %s\n",this->elem_len, cstr);
  }

}
