! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Argument checking
!
  implicit none
  type t
    integer :: i
    integer,allocatable :: j
  end type t

  type(t), save :: x[*]

  call sub1(x%i)
  call sub1(x[1]%i) ! { dg-error "must be a coarray" }
contains
  subroutine sub1(y)
    integer :: y[*]
  end subroutine sub1
end
