/* PR fortran/92470  - to be used with ISO_Fortran_binding_17.f90 */

#include <stdio.h>
#include <assert.h>
#include <ISO_Fortran_binding.h>

void Csub(const CFI_cdesc_t *, size_t, CFI_index_t invalid);

void Csub(const CFI_cdesc_t * dv, size_t locd, CFI_index_t invalid) {

   CFI_index_t lb[1];
   lb[0] = dv->dim[0].lower_bound;
   size_t ld = (size_t)CFI_address(dv, lb);

   if (ld != locd)
     printf ("In C function: CFI_address of dv = %I64x\n", ld);
   assert( ld == locd );

   lb[0] = invalid;
   /* Shall return NULL and produce stderr diagnostic with -fcheck=array.  */
   ld = (size_t)CFI_address(dv, lb);
   assert (ld == 0);

   return;
}
