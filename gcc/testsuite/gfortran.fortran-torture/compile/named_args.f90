! This caused problems because we created a symbol for P while
! trying to parse the argument list as a substring reference.
program named_args
  implicit none
  integer, parameter :: realdp = selected_real_kind(p=8,r=30)
end program
