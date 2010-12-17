! { dg-do compile }

! PR fortran/45776
! Variable definition context checks related to IO.

! Contributed by Daniel Kraft, d@domob.eu.

module m
  implicit none
  integer, protected :: a
  character(len=128), protected :: msg
end module m

program main
  use :: m
  integer :: x
  logical :: bool

  write (*, iostat=a) 42 ! { dg-error "variable definition context" }
  write (*, iomsg=msg) 42 ! { dg-error "variable definition context" }
  read (*, '(I2)', advance='no', size=a) x ! { dg-error "variable definition context" }

  ! These are ok.
  inquire (unit=a)
  inquire (file=msg, id=a, pending=bool)
  inquire (file=msg)

  ! These not, but list is not extensive.
  inquire (unit=1, number=a) ! { dg-error "variable definition context" }
  inquire (unit=1, encoding=msg) ! { dg-error "variable definition context" }
  inquire (unit=1, formatted=msg) ! { dg-error "variable definition context" }

  open (newunit=a, file="foo") ! { dg-error "variable definition context" }
  close (unit=a)
end program main

! { dg-final { cleanup-modules "m" } }
