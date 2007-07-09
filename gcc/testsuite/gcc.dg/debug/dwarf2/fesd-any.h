#include "fesd.h"

struct gstruct_base_ordy_decl_not;
struct gstruct_base_ordy_defn_not { int field_base_ordy_defn_not; };

struct gstruct_base_ordy_decl_ref_base;
struct gstruct_base_ordy_defn_ref_base { int field_base_ordy_defn_ref_base; };
struct gstruct_base_ordy_defn_ptr_base { int field_base_ordy_defn_ptr_base; };
struct gstruct_base_ordy_defn_fld_base { int field_base_ordy_defn_fld_base; };
struct gstruct_base_ordy_defn_var_base {
    struct gstruct_head_ordy_decl_ref_base *field1_base_ordy_defn_var_base_inc;
    struct gstruct_head_ordy_defn_ref_base *field1_base_ordy_defn_var_base_ref;
    struct gstruct_head_ordy_defn_ptr_base *field1_base_ordy_defn_var_base_ptr;
    struct gstruct_head_ordy_defn_fld_base  field1_base_ordy_defn_var_base_fld;
    struct gstruct_base_ordy_decl_ref_base *field2_base_ordy_defn_var_base_inc;
    struct gstruct_base_ordy_defn_ref_base *field2_base_ordy_defn_var_base_ref;
    struct gstruct_base_ordy_defn_ptr_base *field2_base_ordy_defn_var_base_ptr;
    struct gstruct_base_ordy_defn_fld_base  field2_base_ordy_defn_var_base_fld;
};
