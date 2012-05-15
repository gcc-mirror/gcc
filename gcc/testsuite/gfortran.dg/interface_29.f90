! { dg-do compile }
!
! PR 36947: Attributes not fully checked comparing actual vs dummy procedure
!
! Contributed by Tobias Burnus <burnus@net-b.de>

module m
interface foo
  module procedure one, two
end interface foo
contains
subroutine one(op,op2)
    interface
      subroutine op(x, y)
        complex, intent(in)  :: x(:)
        complex, intent(out) :: y(:)
      end subroutine op
      subroutine op2(x, y)
        complex, intent(in)  :: x(:)
        complex, intent(out) :: y(:)
      end subroutine op2
    end interface
end subroutine one
subroutine two(ops,i,j)
    interface
      subroutine op(x, y)
        complex, intent(in)  :: x(:)
        complex, intent(out) :: y(:)
      end subroutine op
    end interface
    real :: i,j
end subroutine two
end module m

module test
contains
subroutine bar()
  use m
  call foo(precond_prop,prop2)
end subroutine bar
  subroutine precond_prop(x, y)
    complex, intent(in)  :: x(:)
    complex, intent(out) :: y(:)
  end subroutine
  subroutine prop2(x, y)
    complex, intent(in)  :: x(:)
    complex, intent(out) :: y(:)
  end subroutine
end module test
