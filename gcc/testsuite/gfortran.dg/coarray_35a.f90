! { dg-do compile }
! { dg-options "-fcoarray=lib" }
! { dg-compile-aux-modules "coarray_35.f90" }
!
! Check that the coarray declared in the module is accessible
! by checking the assembler name
!
! Contributed by Alessandro Fanfarillo.
!
program testmod
  use global_coarrays
  implicit none
  
  integer :: me

  me = this_image()

  b = me

  if(me==1) then
     b(:) = b(:)[2]
     write(*,*) b
  end if

end program testmod

! Check for the symbol of the coarray token (w/o system-dependend prefix)
! { dg-final { scan-assembler "caf_token__global_coarrays_MOD_b" } }
! { dg-final { cleanup-modules "global_coarrays" } }
