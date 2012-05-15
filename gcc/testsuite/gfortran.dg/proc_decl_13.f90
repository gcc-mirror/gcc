! { dg-do run }
! PR fortran/35830
!
module m
contains
  subroutine one(a)
      integer a(:)
      print *, lbound(a), ubound(a), size(a)
      if ((lbound(a,dim=1) /= 1) .or. (ubound(a,dim=1) /= 3)) &
        call abort()
      print *, a
      if (any(a /= [1,2,3])) call abort()
  end subroutine one
end module m

program test
  use m
  implicit none
  call foo1(one)
  call foo2(one)
contains
  subroutine foo1(f)
    ! The following interface block is needed
    ! for NAG f95 as it wrongly does not like
    ! use-associated interfaces for PROCEDURE
    ! (It is not needed for gfortran)
    interface
      subroutine bar(a)
        integer a(:)
      end subroutine
    end interface
    procedure(bar) :: f
    call f([1,2,3]) ! Was failing before
  end subroutine foo1
  subroutine foo2(f)
    interface
      subroutine f(a)
        integer a(:)
      end subroutine
    end interface
    call f([1,2,3]) ! Works
  end subroutine foo2
end program test
