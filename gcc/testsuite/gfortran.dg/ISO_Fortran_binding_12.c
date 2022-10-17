/* Test the fix for PR90093.  */

#include <stdio.h>
#include <math.h>
#include <ISO_Fortran_binding.h>

/* Contributed by Reinhold Bader  <Bader@lrz.de>  */

void foo_opt(CFI_cdesc_t *, float *, int *, int);
void write_res();

float x[34];

int main() {
    CFI_CDESC_T(1) xd;
    CFI_index_t ext[] = {34};
    int sz;

    CFI_establish((CFI_cdesc_t *) &xd, &x, CFI_attribute_other,
		  CFI_type_float, 0, 1, ext);

    foo_opt((CFI_cdesc_t *) &xd, NULL, NULL, 0);
    sz = 12;
    foo_opt(NULL, &x[11], &sz, 1);

    write_res();

    return 0;
}
