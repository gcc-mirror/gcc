/* PR preprocessor/58893 */
/* { dg-do compile } */
/* { dg-options "-include pr58893-0.h -include pr58893-1.h -I${srcdir}/gcc.dg" } */
/* { dg-error "pr58893-1.h: No such file or directory" "" { target *-*-* } 0 } */
/* { dg-prune-output "compilation terminated" } */
