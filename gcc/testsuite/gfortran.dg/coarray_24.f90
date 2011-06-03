! { dg-do compile }
! { dg-options "-fcoarray=single -Wall" }
!
! This program is perfectly valid; however, passing an (allocatable) coarray
! as actual argument to a non-coarray allocatable dummy is doubtful as
! reallocation is not allowed. Thus, an intent(out) dummy should be always
! wrong.
!

integer, allocatable :: myCaf(:)[:]

allocate(myCaf(1)[*])

call doubtful_valid(myCaf)  ! { dg-warning "to allocatable, noncoarray dummy" }
call invalid(myCaf)         ! { dg-error "to allocatable, noncoarray, INTENT.OUT. dummy" }
contains
  subroutine doubtful_valid(x)
    integer, allocatable :: x(:)
    ! Valid as x's allocation status is not touched.
    x(1) = 7
  end subroutine doubtful_valid
  subroutine invalid(y)
    integer, allocatable, intent(out) :: y(:)
    allocate (y(1))
  end subroutine invalid
end
