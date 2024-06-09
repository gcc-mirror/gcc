! { dg-do compile }
! PR fortran/104819
!
! Reject NULL without MOLD as actual to an assumed-rank dummy.
! See also interpretation request at
! https://j3-fortran.org/doc/year/22/22-101r1.txt
!
! Test nested NULL()

program p
  implicit none
  integer, pointer :: a, a3(:,:,:)
  character(10), pointer :: c

  call foo (a)
  call foo (a3)
  call foo (null (a))
  call foo (null (a3))
  call foo (null (null (a)))  ! Valid: nested NULL()s
  call foo (null (null (a3))) ! Valid: nested NULL()s
  call foo (null ())          ! { dg-error "passed to assumed-rank dummy" }

  call str (null (c))
  call str (null (null (c)))
  call str (null ())          ! { dg-error "passed to assumed-length dummy" }
contains
  subroutine foo (x)
    integer, pointer, intent(in) :: x(..)
    print *, rank (x)
  end

  subroutine str (x)
    character(len=*), pointer, intent(in) :: x
  end
end
