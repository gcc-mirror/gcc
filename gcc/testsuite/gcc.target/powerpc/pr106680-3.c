/* { dg-require-effective-target lp64 } */
/* { dg-options "-m64 -mno-powerpc64" } */

/* Verify there is an error message about PowerPC64 requirement.  */

int foo ()
{
  return 1;
}

/* { dg-error "'-m64' requires a PowerPC64 cpu" "PR106680" { target powerpc*-*-linux* powerpc*-*-freebsd* powerpc-*-rtems* powerpc*-*-vxworks* } 0 } */
/* { dg-warning "'-m64' requires PowerPC64 architecture, enabling" "PR106680" { target powerpc*-*-darwin* } 0 } */
/* { dg-warning "'-maix64' requires PowerPC64 architecture remain enabled" "PR106680" { target powerpc*-*-aix* } 0 } */
