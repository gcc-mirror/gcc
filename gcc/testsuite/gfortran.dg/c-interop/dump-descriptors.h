/* Definitions of functions in dump-descriptors.c.  */

#include "ISO_Fortran_binding.h"

extern void dump_CFI_cdesc_t (CFI_cdesc_t *d);
extern void dump_CFI_dim_t (CFI_dim_t *d);
extern void dump_CFI_attribute_t (CFI_attribute_t a);
extern void dump_CFI_index_t (CFI_index_t i);
extern void dump_CFI_rank_t (CFI_rank_t r);
extern void dump_CFI_type_t (CFI_type_t t);

void check_CFI_status (const char *fn, int code);
