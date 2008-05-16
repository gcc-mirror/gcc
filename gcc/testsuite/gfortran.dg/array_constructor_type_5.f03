! { dg-do run }
!
! PR fortran/27997
!
! Array constructor with typespec and small length value.
!
program test
  implicit none
  character(15) :: a(3)
  a =  (/ character(len=3) :: 'Takata', 'Tanaka', 'Hayashi' /)
  if(     a(1) /= 'Tak'  .or. a(1)(4:4)   /= achar(32) &
                         .or. a(1)(15:15) /= achar(32) &
     .or. a(2) /= 'Tan'  .or. a(2)(4:4)   /= achar(32) &
                         .or. a(2)(15:15) /= achar(32) &
     .or. a(3) /= 'Hay'  .or. a(3)(4:4)   /= achar(32) &
                         .or. a(3)(15:15) /= achar(32))&
   call abort()
end program test
