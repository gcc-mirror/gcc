/* { dg-do compile } */
/* { dg-options "-fprofile-arcs" } */
/* { dg-require-profiling "-fprofile-generate" } */

extern void relocate_kernel();
void machine_kexec(void *control_page)
{ 
  __builtin_memcpy(control_page, relocate_kernel, 2048);
}
