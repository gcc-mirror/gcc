! { dg-do compile }
! fortran/pr20846
program inquire_8
  character(len=20) :: n = 'data'
  integer :: d = 23
  logical a
  inquire(file=n,unit=d,opened=a) ! { dg-error "contain both FILE and UNIT" }
  inquire(unit=d,file=n,opened=a) ! { dg-error "contain both FILE and UNIT" }
  inquire(opened=a)               ! { dg-error "requires either FILE or UNIT" }
end program inquire_8
