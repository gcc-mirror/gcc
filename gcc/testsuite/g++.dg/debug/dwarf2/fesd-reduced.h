#include "fesd.h"

struct gstruct_base_ordy_decl_not;
struct gstruct_base_ordy_defn_not { int field_base_ordy_defn_not; };

struct gstruct_base_ordy_decl_ref_base;
struct gstruct_base_ordy_defn_ref_base { int field_base_ordy_defn_ref_base; };
struct gstruct_base_ordy_defn_ptr_base { int field_base_ordy_defn_ptr_base; };
struct gstruct_base_ordy_defn_fld_base { int field_base_ordy_defn_fld_base; };
struct gstruct_base_ordy_defn_var_base {
    gstruct_head_ordy_decl_ref_base *field1_base_ordy_defn_var_base_inc;
    gstruct_head_ordy_defn_ref_base *field1_base_ordy_defn_var_base_ref;
    gstruct_head_ordy_defn_ptr_base *field1_base_ordy_defn_var_base_ptr;
    gstruct_head_ordy_defn_fld_base  field1_base_ordy_defn_var_base_fld;
    gstruct_base_ordy_decl_ref_base *field2_base_ordy_defn_var_base_inc;
    gstruct_base_ordy_defn_ref_base *field2_base_ordy_defn_var_base_ref;
    gstruct_base_ordy_defn_ptr_base *field2_base_ordy_defn_var_base_ptr;
    gstruct_base_ordy_defn_fld_base  field2_base_ordy_defn_var_base_fld;
};

template< typename T > struct gstruct_base_tmpl_decl_not;
template< typename T > struct gstruct_base_tmpl_defn_not
{ int field_base_tmpl_defn_not; };

template< typename T > struct gstruct_base_tmpl_decl_ref_base;
template< typename T > struct gstruct_base_tmpl_defn_ref_base
{ int field_base_tmpl_defn_ref_base; };
template< typename T > struct gstruct_base_tmpl_defn_ptr_base
{ int field_base_tmpl_defn_ptr_base; };
template< typename T > struct gstruct_base_tmpl_defn_fld_base
{ int field_base_tmpl_defn_fld_base; };
template< typename T > struct gstruct_base_tmpl_defn_var_base {
    gstruct_head_tmpl_decl_ref_base< T > *field1_base_tmpl_defn_var_base_inc;
    gstruct_head_tmpl_defn_ref_base< T > *field1_base_tmpl_defn_var_base_ref;
    gstruct_head_tmpl_defn_ptr_base< T > *field1_base_tmpl_defn_var_base_ptr;
    gstruct_head_tmpl_defn_fld_base< T >  field1_base_tmpl_defn_var_base_fld;
    gstruct_base_tmpl_decl_ref_base< T > *field2_base_tmpl_defn_var_base_inc;
    gstruct_base_tmpl_defn_ref_base< T > *field2_base_tmpl_defn_var_base_ref;
    gstruct_base_tmpl_defn_ptr_base< T > *field2_base_tmpl_defn_var_base_ptr;
    gstruct_base_tmpl_defn_fld_base< T >  field2_base_tmpl_defn_var_base_fld;
};

