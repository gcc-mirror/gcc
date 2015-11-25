! Fortran
! { dg-do compile { target { powerpc*-*-* } } }
! { dg-require-effective-target powerpc_vsx_ok } 
! { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } 
! { dg-options "-O2 -mcpu=power7 -ffast-math" } 
! { dg-final { scan-assembler-times "xsrdpi" 2 } } 

	subroutine test_nint(x4,x8)
          real(4) x4
          real(8) x8
          print *, nint(x4), idnint(x8)
	end subroutine test_nint
