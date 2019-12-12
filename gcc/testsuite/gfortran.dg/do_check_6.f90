! { dg-do compile }
!
! PR fortran/54958
!
module m
  integer, protected :: i
  integer :: j
end module m

subroutine test1()
  use m
  implicit none
  integer :: A(5)
  ! Valid: data-implied-do (has a scope of the statement or construct)
  DATA (A(i), i=1,5)/5*42/ ! OK

  ! Valid: ac-implied-do (has a scope of the statement or construct)
  print *, [(i, i=1,5 )] ! OK

  ! Valid: index-name (has a scope of the statement or construct)
  forall (i = 1:5) ! OK
  end forall

  ! Valid: index-name (has a scope of the statement or construct)
  do concurrent (i = 1:5) ! OK
  end do

  ! Invalid: io-implied-do
  print *, (i, i=1,5 ) ! { dg-error "PROTECTED and cannot appear in a variable definition context .iterator variable." }

  ! Invalid: do-variable in a do-stmt
  do i = 1, 5 ! { dg-error "PROTECTED and cannot appear in a variable definition context .iterator variable." }
  end do
end subroutine test1

subroutine test2(i)
  implicit none
  integer, intent(in) :: i
  integer :: A(5)
  ! Valid: data-implied-do (has a scope of the statement or construct)
  DATA (A(i), i=1,5)/5*42/ ! OK

  ! Valid: ac-implied-do (has a scope of the statement or construct)
  print *, [(i, i=1,5 )] ! OK

  ! Valid: index-name (has a scope of the statement or construct)
  forall (i = 1:5) ! OK
  end forall

  ! Valid: index-name (has a scope of the statement or construct)
  do concurrent (i = 1:5) ! OK
  end do

  ! Invalid: io-implied-do
  print *, (i, i=1,5 ) ! { dg-error "INTENT.IN. in variable definition context .iterator variable." }

  ! Invalid: do-variable in a do-stmt
  do i = 1, 5 ! { dg-error "INTENT.IN. in variable definition context .iterator variable." }
  end do
end subroutine test2

pure subroutine test3()
  use m
  implicit none
  integer :: A(5)
  !DATA (A(j), j=1,5)/5*42/ ! Not allowed in pure

  ! Valid: ac-implied-do (has a scope of the statement or construct)
  A = [(j, j=1,5 )] ! OK

  ! Valid: index-name (has a scope of the statement or construct)
  forall (j = 1:5) ! OK
  end forall

  ! Valid: index-name (has a scope of the statement or construct)
  do concurrent (j = 1:5) ! OK
  end do

  ! print *, (j, j=1,5 ) ! I/O not allowed in PURE

  ! Invalid: do-variable in a do-stmt
  do j = 1, 5 ! { dg-error "variable definition context .iterator variable. at .1. in PURE procedure" }
  end do
end subroutine test3
