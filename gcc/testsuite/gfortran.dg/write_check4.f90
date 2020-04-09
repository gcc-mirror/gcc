! { dg-do compile }
!
! PR fortran/35840 
!
! The asynchronous specifier for a data transfer statement shall be 
! an initialization expression
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
  character(2) :: no
  no = "no"
  open (unit=10, asynchronous = no)              ! Ok, it isn't a transfer stmt
  write(*,*, asynchronous="Y"//"E"//trim("S  ")) ! Ok, it is an init expr
  write(*,*, asynchronous=no)  ! { dg-error "does not reduce to a constant expression" }
  read (*,*, asynchronous="Y"//"e"//trim("S  "))
  read (*,*, asynchronous=no)  ! { dg-error "does not reduce to a constant expression" }
end
