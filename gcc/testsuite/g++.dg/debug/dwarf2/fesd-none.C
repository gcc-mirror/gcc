// { dg-do compile }
// { dg-options "-gdwarf-2 -dA -femit-struct-debug-detailed=none -fno-eliminate-unused-debug-symbols" }
// { dg-final { scan-assembler "timespec.*DW_AT_name" } }
// { dg-final { scan-assembler-not "tv_sec.*DW_AT_name" } }
// { dg-final { scan-assembler-not "tv_nsec.*DW_AT_name" } }
// { dg-final { scan-assembler "itimerspec.*DW_AT_name" } }
// { dg-final { scan-assembler-not "it_interval.*DW_AT_name" } }
// { dg-final { scan-assembler-not "it_value.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_head_ordy_defn_ref_head.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_ordy_defn_ref_head.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_head_ordy_defn_ptr_head.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_ordy_defn_ptr_head.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_head_ordy_defn_fld_head.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_ordy_defn_fld_head.*DW_AT_name" } }
// { dg-final { scan-assembler "gstruct_head_ordy_defn_var_head.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_ordy_defn_var_head_inc.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_ordy_defn_var_head_ref.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_ordy_defn_var_head_ptr.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_ordy_defn_var_head_fld.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_head_ordy_decl_ref_head.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_head_ordy_defn_ref_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_ordy_defn_ref_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_head_ordy_defn_ptr_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_ordy_defn_ptr_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_head_ordy_defn_fld_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_ordy_defn_fld_base.*DW_AT_name" } }
// { dg-final { scan-assembler "gstruct_head_ordy_defn_var_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_ordy_defn_var_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_head_tmpl_defn_fld_head<int>.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_tmpl_defn_fld_head.*DW_AT_name" } }
// { dg-final { scan-assembler "gstruct_head_tmpl_defn_var_head<int>.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_tmpl_defn_var_head_inc.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_tmpl_defn_var_head_ref.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_tmpl_defn_var_head_ptr.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_tmpl_defn_var_head_fld.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_head_tmpl_decl_ref_head<int>.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_head_tmpl_defn_ref_head<int>.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_head_tmpl_defn_ptr_head<int>.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_tmpl_defn_ptr_head.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_base_ordy_defn_ref_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_base_ordy_defn_ref_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_base_ordy_defn_ptr_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_base_ordy_defn_ptr_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_base_ordy_defn_fld_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_base_ordy_defn_fld_base.*DW_AT_name" } }
// { dg-final { scan-assembler "gstruct_base_ordy_defn_var_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field1_base_ordy_defn_var_base_inc.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field1_base_ordy_defn_var_base_ref.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field1_base_ordy_defn_var_base_ptr.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field1_base_ordy_defn_var_base_fld.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field2_base_ordy_defn_var_base_inc.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field2_base_ordy_defn_var_base_ref.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field2_base_ordy_defn_var_base_ptr.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field2_base_ordy_defn_var_base_fld.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_head_ordy_decl_ref_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_base_ordy_decl_ref_base.*DW_AT_name" } }
// { dg-final { scan-assembler "gstruct_head_tmpl_defn_var_base<int>.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_tmpl_defn_var_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_head_tmpl_defn_fld_base<int>.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_tmpl_defn_fld_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_base_tmpl_defn_fld_base<int>.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_base_tmpl_defn_fld_base.*DW_AT_name" } }
// { dg-final { scan-assembler "gstruct_base_tmpl_defn_var_base<int>.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field1_base_tmpl_defn_var_base_inc.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field1_base_tmpl_defn_var_base_ref.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field1_base_tmpl_defn_var_base_ptr.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field1_base_tmpl_defn_var_base_fld.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field2_base_tmpl_defn_var_base_inc.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field2_base_tmpl_defn_var_base_ref.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field2_base_tmpl_defn_var_base_ptr.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field2_base_tmpl_defn_var_base_fld.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_head_tmpl_decl_ref_base<int>.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_head_tmpl_defn_ref_base<int>.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_head_tmpl_defn_ptr_base<int>.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_head_tmpl_defn_ptr_base.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_base_tmpl_decl_ref_base<int>.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_base_tmpl_defn_ref_base<int>.*DW_AT_name" } }
// { dg-final { scan-assembler-not "gstruct_base_tmpl_defn_ptr_base<int>.*DW_AT_name" } }
// { dg-final { scan-assembler-not "field_base_tmpl_defn_ptr_base.*DW_AT_name" } }
# 1 "fesd-none.C"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "fesd-none.C"

//#include "time.h"
# 1 "time.h" 1 3 4
struct timespec
  {
    long int tv_sec;
    long int tv_nsec;
  };

struct itimerspec
  {
    struct timespec it_interval;
    struct timespec it_value;
  };

# 6 "fesd-none.C" 2

struct timespec base_var8;
struct itimerspec *base_var9;

#include "fesd-none.h"

struct gstruct_head_ordy_defn_var_base base_var1;
struct gstruct_base_ordy_defn_var_base base_var2;

struct gstruct_head_tmpl_defn_var_base< int > base_var5;
struct gstruct_base_tmpl_defn_var_base< int > base_var6;

int base_function() {
    return 0
+ base_var1.field_head_ordy_defn_var_base
+ base_var2.field1_base_ordy_defn_var_base_ptr->field_head_ordy_defn_ptr_base
+ base_var2.field1_base_ordy_defn_var_base_fld.field_head_ordy_defn_fld_base
+ base_var2.field2_base_ordy_defn_var_base_ptr->field_base_ordy_defn_ptr_base
+ base_var2.field2_base_ordy_defn_var_base_fld.field_base_ordy_defn_fld_base
+ base_var5.field_head_tmpl_defn_var_base
+ base_var6.field1_base_tmpl_defn_var_base_ptr->field_head_tmpl_defn_ptr_base
+ base_var6.field1_base_tmpl_defn_var_base_fld.field_head_tmpl_defn_fld_base
+ base_var6.field2_base_tmpl_defn_var_base_ptr->field_base_tmpl_defn_ptr_base
+ base_var6.field2_base_tmpl_defn_var_base_fld.field_base_tmpl_defn_fld_base
;
}

