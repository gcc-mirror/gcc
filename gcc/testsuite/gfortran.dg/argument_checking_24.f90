! { dg-do compile }
! PR 92004 - checks in the absence of an explicit interface between
! array elements and arrays
module x
  implicit none
  type t
     real :: x
  end type t
  type tt
     real :: x(2)
  end type tt
  type pointer_t
     real, pointer :: x(:)
  end type pointer_t
  type alloc_t
     real, dimension(:), allocatable :: x
  end type alloc_t
contains
  subroutine foo(a)
    real, dimension(:) :: a
    real, dimension(2), parameter :: b = [1.0, 2.0]
    real, dimension(10) :: x
    type (t), dimension(1) :: vv
    type (pointer_t) :: pointer_v
    real, dimension(:), pointer :: p
    call invalid_1(a(1))  ! { dg-error "Rank mismatch" }
    call invalid_1(a) ! { dg-error "Rank mismatch" }
    call invalid_2(a) ! { dg-error "Element of assumed-shape or pointer" }
    call invalid_2(a(1))  ! { dg-error "Element of assumed-shape or pointer" }
    call invalid_3(b) ! { dg-error "Rank mismatch" }
    call invalid_3(1.0) ! { dg-error "Rank mismatch" }
    call invalid_4 (vv(1)%x) ! { dg-error "Rank mismatch" }
    call invalid_4 (b) ! { dg-error "Rank mismatch" }w
    call invalid_5 (b) ! { dg-error "Rank mismatch" }
    call invalid_5 (vv(1)%x) ! { dg-error "Rank mismatch" }
    call invalid_6 (x) ! { dg-error "can not correspond to actual argument" }
    call invalid_6 (pointer_v%x(1)) ! { dg-error "can not correspond to actual argument" }
    call invalid_7 (pointer_v%x(1)) ! { dg-error "Rank mismatch" }
    call invalid_7 (x) ! { dg-error "Rank mismatch" }
    call invalid_8 (p(1)) ! { dg-error "Rank mismatch" }
    call invalid_8 (x) ! { dg-error "Rank mismatch" }
    call invalid_9 (x) ! { dg-error "can not correspond to actual argument" }
    call invalid_9 (p(1)) ! { dg-error "can not correspond to actual argument" }
  end subroutine foo

  subroutine bar(a, alloc)
    real, dimension(*) :: a
    real, dimension(2), parameter :: b = [1.0, 2.0]
    type (alloc_t), pointer :: alloc
    type (tt) :: tt_var
    ! None of the ones below should issue an error.
    call valid_1 (a)
    call valid_1 (a(1))
    call valid_2 (a(1))
    call valid_2 (a)
    call valid_3 (b)
    call valid_3 (b(1))
    call valid_4 (tt_var%x)
    call valid_4 (tt_var%x(1))
    call valid_5 (alloc%x(1))
    call valid_5 (a)
  end subroutine bar
end module x
