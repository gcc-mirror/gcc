! { dg-do compile }
! { dg-options "-O2 -Wextra -fdump-tree-optimized" }
!
! PR fortran/122709 - bogus warning for contiguous pointer assignment
!                     to select type target
!
! Contributed by <mscfd at gmx dot net>

module sc_mod
  implicit none
  public

  type :: t
     integer :: i = 0
  end type t

  type :: s
     class(t), dimension(:), contiguous, pointer :: p => null()
  end type s

contains

  subroutine foo(x)
    class(s), intent(in) :: x
    type(t), dimension(:), contiguous, pointer :: q
    select type (p_ => x%p)
    type is (t)
       q => p_
       if (.not. is_contiguous(x%p)) stop 1
       if (.not. is_contiguous(p_))  stop 2     ! Should get optimized out
       if (.not. is_contiguous(q))   stop 3
       write(*,*) 'is contiguous: ', is_contiguous(x%p), &
            is_contiguous(p_), is_contiguous(q)
    end select
  end subroutine foo

end module sc_mod

program select_contiguous
  use sc_mod
  implicit none

  type(s) :: x

  allocate(t :: x%p(1:10))
  call foo(x)
  deallocate(x%p)

end program select_contiguous

! { dg-final { scan-tree-dump-not "_gfortran_stop_numeric" "optimized" } }
