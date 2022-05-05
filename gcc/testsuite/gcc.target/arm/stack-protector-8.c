/* { dg-require-effective-target arm_hard_vfp_ok }  */
/* { dg-require-effective-target arm_arch_v7a_ok } */
/* { dg-do compile } */
/* { dg-error "needs a hardware TLS register" "missing error when using TLS stack protector without hardware TLS register" { target *-*-* } 0 } */
/* { dg-options "-fstack-protector-all -Os -mstack-protector-guard=tls -mtp=soft" } */

int foo;
