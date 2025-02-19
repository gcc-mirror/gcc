! { dg-do run }
! PR fortran/118080
!
! Test passing of scalar derived types (user-defined or ISO_C_BINDING)
! to dummy argument with OPTIONAL + VALUE attribute
!
! Original/initial testcase by Tobias Burnus

module m
  use iso_c_binding
  implicit none(type,external)
  logical is_present
contains
  subroutine f(x)
    ! void f (void * x, logical(kind=1) .x)  - 2nd arg = is-present flag
    type(c_ptr), optional, value :: x
    if (present(x) .neqv. is_present) stop 1
    if (present(x)) then
      block
        integer, pointer :: ptr
        call c_f_pointer(x,ptr)
        if (ptr /= 55) stop 2
      end block
    endif
  end
end

module m0
  use m
  implicit none(type,external)
contains
  subroutine test_pr118080
    type(c_ptr) :: a
    integer, target :: x
    a = c_loc(x)
    x = 55

    is_present = .true.
    call f(a)

    is_present = .false.
    call f()

    ! Trying again after the absent call:
    is_present = .true.
    call f(a)

    print *, "Passed original test"
  end subroutine test_pr118080
end

! Exercise ISO_C_BINDING uses
module m1
  use iso_c_binding, only: c_ptr, c_funptr, C_NULL_PTR, C_NULL_FUNPTR
  implicit none
  logical :: is_present = .false.
  integer :: base = 0
contains
  subroutine test_c ()
    type(c_ptr)    :: x = C_NULL_PTR
    type(c_funptr) :: y = C_NULL_FUNPTR

    is_present = .true.
    base = 10
    ! Tests with c_ptr:
    call f_c (x)
    call f_c_opt (x)
    call f_c_val (x)
    call f_c_opt_val (x)
    call f_c2_opt (x)
    call f_c2_opt_val (x)

    ! Tests with c_funptr:
    call g_c (y)
    call g_c_opt (y)
    call g_c_val (y)
    call g_c_opt_val (y)
    call g_c2_opt (y)
    call g_c2_opt_val (y)

    ! Elemental subroutine calls:
    base = 20
    call f_c ([x])
    call f_c_opt ([x])
    call f_c_val ([x])
    call f_c_opt_val ([x])
    call f_c2_opt ([x])
    call f_c2_opt_val ([x])

    call g_c ([y])
    call g_c_opt ([y])
    call g_c_val ([y])
    call g_c_opt_val ([y])
    call g_c2_opt ([y])
    call g_c2_opt_val ([y])

    is_present = .false.
    base = 30
    call f_c_opt ()
    call f_c_opt_val ()
    call f_c2_opt ()
    call f_c2_opt_val ()

    call g_c_opt ()
    call g_c_opt_val ()
    call g_c2_opt ()
    call g_c2_opt_val ()

    print *, "Passed test_c"
  end subroutine test_c

  elemental subroutine f_c (x)
    type(c_ptr), intent(in) :: x
  end
  !
  elemental subroutine f_c_val (x)
    type(c_ptr), value :: x
    call f_c (x)
  end
  !
  elemental subroutine f_c_opt (x)
    type(c_ptr), intent(in), optional :: x
    if (present (x) .neqv. is_present) error stop base+1
  end
  !
  elemental subroutine f_c_opt_val (x)
    type(c_ptr), value, optional :: x
    if (present (x) .neqv. is_present) error stop base+2
  end
  !
  elemental subroutine f_c2_opt_val (x)
    type(c_ptr), value, optional :: x
    if (present (x) .neqv. is_present) error stop base+3
    call f_c_opt (x)
    call f_c_opt_val (x)
    if (present (x)) call f_c (x)
    if (present (x)) call f_c_val (x)
  end
  !
  elemental subroutine f_c2_opt (x)
    type(c_ptr), intent(in), optional :: x
    if (present (x) .neqv. is_present) error stop base+4
    call f_c_opt_val (x)
    call f_c2_opt_val (x)
  end

  elemental subroutine g_c (x)
    type(c_funptr), intent(in) :: x
  end
  !
  elemental subroutine g_c_val (x)
    type(c_funptr), value :: x
    call g_c (x)
  end
  !
  elemental subroutine g_c_opt (x)
    type(c_funptr), intent(in), optional :: x
    if (present (x) .neqv. is_present) error stop base+6
  end
  !
  elemental subroutine g_c_opt_val (x)
    type(c_funptr), value, optional :: x
    if (present (x) .neqv. is_present) error stop base+7
  end
  !
  elemental subroutine g_c2_opt_val (x)
    type(c_funptr), value, optional :: x
    if (present (x) .neqv. is_present) error stop base+8
    call g_c_opt (x)
    call g_c_opt_val (x)
    if (present (x)) call g_c (x)
    if (present (x)) call g_c_val (x)
  end
  !
  elemental subroutine g_c2_opt (x)
    type(c_funptr), intent(in), optional :: x
    if (present (x) .neqv. is_present) error stop base+9
    call g_c_opt_val (x)
    call g_c2_opt_val (x)
  end
  !
end

! Exercise simple user-defined types
module m2
  implicit none

  type t1
     character(42) :: c = ""
     logical       :: l = .false.
  end type t1

  type, bind(c) :: t2
     real    :: r(8) = 0.
     complex :: c(4) = 0.
     integer :: i    = 0
  end type t2

  logical :: is_present = .false.
  integer :: base = 0
contains
  subroutine test_t ()
    type(t1) :: x
    type(t2) :: y

    x% c = "foo"

    is_present = .true.
    base = 50
    ! Tests with t1:
    call f_c (x)
    call f_c_opt (x)
    call f_c_val (x)
    call f_c_opt_val (x)
    call f_c2_opt (x)
    call f_c2_opt_val (x)

    ! Tests with t2:
    call g_c (y)
    call g_c_opt (y)
    call g_c_val (y)
    call g_c_opt_val (y)
    call g_c2_opt (y)
    call g_c2_opt_val (y)

    ! Elemental subroutine calls:
    base = 60
    call f_c ([x])
    call f_c_opt ([x])
    call f_c_val ([x])
    call f_c_opt_val ([x])
    call f_c2_opt ([x])
    call f_c2_opt_val ([x])

    call g_c ([y])
    call g_c_opt ([y])
    call g_c_val ([y])
    call g_c_opt_val ([y])
    call g_c2_opt ([y])
    call g_c2_opt_val ([y])

    is_present = .false.
    base = 70
    call f_c_opt ()
    call f_c_opt_val ()
    call f_c2_opt ()
    call f_c2_opt_val ()

    call g_c_opt ()
    call g_c_opt_val ()
    call g_c2_opt ()
    call g_c2_opt_val ()

    print *, "Passed test_t"
  end subroutine test_t

  elemental subroutine f_c (x)
    type(t1), intent(in) :: x
    if (x% c /= "foo") error stop base
  end
  !
  elemental subroutine f_c_val (x)
    type(t1), value :: x
    if (x% c /= "foo") error stop base+5
    call f_c (x)
  end
  !
  elemental subroutine f_c_opt (x)
    type(t1), intent(in), optional :: x
    if (present (x) .neqv. is_present) error stop base+1
  end
  !
  elemental subroutine f_c_opt_val (x)
    type(t1), value, optional :: x
    if (present (x) .neqv. is_present) error stop base+2
  end
  !
  elemental subroutine f_c2_opt_val (x)
    type(t1), value, optional :: x
    if (present (x) .neqv. is_present) error stop base+3
    call f_c_opt (x)
    call f_c_opt_val (x)
    if (present (x)) call f_c (x)
    if (present (x)) call f_c_val (x)
  end
  !
  elemental subroutine f_c2_opt (x)
    type(t1), intent(in), optional :: x
    if (present (x) .neqv. is_present) error stop base+4
    call f_c_opt_val (x)
    call f_c2_opt_val (x)
  end

  elemental subroutine g_c (x)
    type(t2), intent(in) :: x
  end
  !
  elemental subroutine g_c_val (x)
    type(t2), value :: x
    call g_c (x)
  end
  !
  elemental subroutine g_c_opt (x)
    type(t2), intent(in), optional :: x
    if (present (x) .neqv. is_present) error stop base+6
  end
  !
  elemental subroutine g_c_opt_val (x)
    type(t2), value, optional :: x
    if (present (x) .neqv. is_present) error stop base+7
  end
  !
  elemental subroutine g_c2_opt_val (x)
    type(t2), value, optional :: x
    if (present (x) .neqv. is_present) error stop base+8
    call g_c_opt (x)
    call g_c_opt_val (x)
    if (present (x)) call g_c (x)
    if (present (x)) call g_c_val (x)
  end
  !
  elemental subroutine g_c2_opt (x)
    type(t2), intent(in), optional :: x
    if (present (x) .neqv. is_present) error stop base+9
    call g_c_opt_val (x)
    call g_c2_opt_val (x)
  end
end

program pr118080
  use m0
  use m1
  use m2
  implicit none
  call test_pr118080 ()
  call test_c()
  call test_t()
end
