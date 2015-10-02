! { dg-do compile }
! PR fortran/66979
program p
  implicit none
  integer::i
  flush (iostat=i) ! { dg-error "UNIT number missing" }
end program p
