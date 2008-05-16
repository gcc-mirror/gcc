! { dg-do run }
!
! PR fortran/27997
!
! Array constructor with typespec, length parameter.
!
program test
  implicit none
  character(15) :: a(3)
  a =  (/ character(len=7) :: 'Takata', 'Tanaka', 'Hayashi' /)
  if ( len([ character(len=7) :: ]) /= 7) call abort()
  if ( size([ integer :: ]) /= 0) call abort()
  if(     a(1) /= 'Takata'  .or. a(1)(7:7)   /= achar(32) &
                            .or. a(1)(15:15) /= achar(32) &
     .or. a(2) /= 'Tanaka'  .or. a(2)(7:7)   /= achar(32) &
                            .or. a(2)(15:15) /= achar(32) &
     .or. a(3) /= 'Hayashi' .or. a(3)(8:8)   /= achar(32) &
                            .or. a(3)(15:15) /= achar(32))&
   call abort()
end program test
