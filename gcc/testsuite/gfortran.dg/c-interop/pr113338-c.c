/* PR fortran/113338.  */

#include <ISO_Fortran_binding.h>

extern void f_proc(CFI_cdesc_t* x);

extern void c_proc(CFI_cdesc_t* x)
{
    f_proc(x);
}
