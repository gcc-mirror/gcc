! { dg-do compile { target { ! *-*-* } } }
! SKIP THIS FILE
!
! Used by codimension_2.f90
!
! Check that the coarray declared in the module is accessible
! by doing a link test
!
! Contributed by Alessandro Fanfarillo.
!
program testmod
  use global_coarrays
  implicit none
  external ttest
  
  integer :: me

  me = this_image()

  b = me

  if(me==1) then
     b(:) = b(:)[2]
     write(*,*) b
  elseif (me == 3) then
     call ttest()
  end if

end program testmod
