/*
where from: base = matching base; head = other header
what kind:  ordy = ordinary struct; tmpl = template struct
definition: decl = incomplete declaration; defn = full definition
how used:   not = not used; ref = by ref; ptr = through pointer;
            fld = as field; var = as variable
from where: base = from base; head = other header
*/

struct gstruct_head_ordy_decl_not;
struct gstruct_head_ordy_defn_not { int field_head_ordy_defn_not; };

struct gstruct_head_ordy_decl_ref_head;
struct gstruct_head_ordy_defn_ref_head { int field_head_ordy_defn_ref_head; };
struct gstruct_head_ordy_defn_ptr_head { int field_head_ordy_defn_ptr_head; };
struct gstruct_head_ordy_defn_fld_head { int field_head_ordy_defn_fld_head; };
struct gstruct_head_ordy_defn_var_head {
    struct gstruct_head_ordy_decl_ref_head *field_head_ordy_defn_var_head_inc;
    struct gstruct_head_ordy_defn_ref_head *field_head_ordy_defn_var_head_ref;
    struct gstruct_head_ordy_defn_ptr_head *field_head_ordy_defn_var_head_ptr;
    struct gstruct_head_ordy_defn_fld_head  field_head_ordy_defn_var_head_fld;
};
extern struct gstruct_head_ordy_defn_var_head head_var1;

struct gstruct_head_ordy_decl_ref_base;
struct gstruct_head_ordy_defn_ref_base { int field_head_ordy_defn_ref_base; };
struct gstruct_head_ordy_defn_ptr_base { int field_head_ordy_defn_ptr_base; };
struct gstruct_head_ordy_defn_fld_base { int field_head_ordy_defn_fld_base; };
struct gstruct_head_ordy_defn_var_base { int field_head_ordy_defn_var_base; };

inline int head_function() {
    return 0
+ head_var1.field_head_ordy_defn_var_head_ptr->field_head_ordy_defn_ptr_head
+ head_var1.field_head_ordy_defn_var_head_fld.field_head_ordy_defn_fld_head
;
}
