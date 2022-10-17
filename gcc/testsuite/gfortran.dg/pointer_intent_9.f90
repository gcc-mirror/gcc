! { dg-do compile }
! PR fortran/103418
! Validate checks for dummy arguments with INTENT(IN), pointer attribute

module m
  type t
     real, pointer :: a, b(:)
  end type t
contains
  subroutine s1 (a, b, c, d, e)
    real,    pointer, intent(in) :: a, b(:)
    type(t),          intent(in) :: c
    class(t),         intent(in) :: d
    type(t), pointer, intent(in) :: e
    real, pointer :: pa, pb(:)
    call random_number (a)    ! legal
    call random_number (b)
    call cpu_time      (a)
    call system_clock  (count_rate=a)
    call random_number (c% a)
    call random_number (c% b)
    call random_number (d% a)
    call random_number (d% b)
    call random_number (e% a)
    call random_number (e% b)
    call move_alloc (a, pa)   ! { dg-error "must be ALLOCATABLE" }
    call move_alloc (b, pb)   ! { dg-error "must be ALLOCATABLE" }
    allocate (a)              ! { dg-error "pointer association context" }
    allocate (b(10))          ! { dg-error "pointer association context" }
    allocate (c% a)           ! { dg-error "pointer association context" }
    allocate (c% b(10))       ! { dg-error "pointer association context" }
  end subroutine s1
end module
