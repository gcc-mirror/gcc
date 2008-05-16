! { dg-do run }
! { dg-options "-fbounds-check" }
!
! PR fortran/27997
!
! Array constructor with typespec.
!
program test
  character(15) :: a(3)
  character(10), volatile :: b(3)
  b(1) = 'Takata'
  b(2) = 'Tanaka'
  b(3) = 'Hayashi'

  a =  (/ character(len=7) :: trim(b(1)), trim(b(2)), trim(b(3)) /)
  if (a(1) /= 'Takata' .or. a(2) /= 'Tanaka' .or. a(3) /= 'Hayashi') then
    call abort ()
  end if

  a =  (/ character(len=2) :: trim(b(1)), trim(b(2)), trim(b(3)) /)
  if (a(1) /= 'Ta' .or. a(2) /= 'Ta' .or. a(3) /= 'Ha') then
    call abort ()
  end if

  a =  (/ character(len=8) :: trim(b(1)), trim(b(2)), trim(b(3)) /)
  if (a(1) /= 'Takata' .or. a(2) /= 'Tanaka' .or. a(3) /= 'Hayashi') then
    call abort ()
  end if

end program test
