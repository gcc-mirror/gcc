/* Verify the typedef chains are preserved.
   DW_TAG_const_type for "const u16" chains to u16 typedef and doesnot
   skip to underlying __u16 typedef */

/* { dg-do compile } */
/* { dg-options "-gdwarf -dA" } */

typedef short __u16;
typedef __u16 u16;
__attribute__((btf_type_tag(""))) u16 __softirq_pending;
const u16 perm;

/* The exact failing pattern is hard to encode with TCL regex machinery,
   so resort to an indirect way: in the buggy output, const_type points
   directly to __u16, and only one u16 typedef DIE is emitted (for the
   btf_type_tag use). In the fixed output, const_type points to a u16
   typedef, creating a second u16 typedef DIE.
/* { dg-final { scan-assembler-times "(DW_AT_name: \"u16\"|\"u16..\"\[^\\r\\n\]*DW_AT_name)" 2 } } */
