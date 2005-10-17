! { dg-do run }
! { dg-options "-O0" }
!
! Tests fix for PR21459 - This is the original example.
!
program format_string
  implicit none
  character(len=*), parameter :: rform='(F15.5)', &
  cform="(' (', F15.5, ',' F15.5, ') ')"
  call print_a_number(cform)
contains
subroutine print_a_number(style)
  character(len=*) :: style
  write(*, style) cmplx(42.0, 99.0) ! { dg-output "99.00000" }
end subroutine print_a_number
end program format_string
