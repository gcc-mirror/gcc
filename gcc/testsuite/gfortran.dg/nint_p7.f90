! Fortran
! { dg-do compile { target { powerpc*-*-* } } }
! { dg-options "-O2 -mdejagnu-cpu=power7 -ffast-math" } 
! { dg-require-effective-target powerpc_vsx } 
! { dg-require-effective-target has_arch_ppc64 } 
! { dg-final { scan-assembler-times "xsrdpi" 2 } } 

	subroutine test_nint(x4,x8)
          real(4) x4
          real(8) x8
          print *, nint(x4), idnint(x8)
	end subroutine test_nint
