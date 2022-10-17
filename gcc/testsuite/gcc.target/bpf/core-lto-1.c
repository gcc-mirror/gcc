/* Test -mco-re with -flto.
  
   -mco-re is used to generate information for BPF CO-RE usecase. To support
   the generataion of the .BTF and .BTF.ext sections in GCC, -flto is disabled
   with -mco-re.  */

/* { dg-do compile } */
/* { dg-message "sorry, unimplemented: BPF CO-RE does not support LTO" "" { target bpf-*-* } 0 } */
/* { dg-options "-gbtf -mco-re -flto" } */
