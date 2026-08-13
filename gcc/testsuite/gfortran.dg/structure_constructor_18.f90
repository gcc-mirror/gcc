! { dg-do run }
!
! Test the fix for PR121683 and a variant from Jerry Delisle. Explicit
! deallocations were added, when both original and variant were stand
! alone programs.
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!            and Jerry Delisle  <jvdelisle@gcc.gnu.org>
!
module mod
  implicit none
  type, abstract :: func
  end type
  type, extends(func) :: poly
    real, allocatable :: array(:)
  end type
  type, extends(func) :: func_deriv
    class(func), allocatable :: f
  end type

contains

  subroutine alloc_deriv(f, df)
    class(func), intent(in) :: f
    class(func), allocatable, intent(out) :: df
    allocate(df, source=func_deriv(f)) ! THIS CORRUPTS F
  end subroutine

end module

program fubar
  call original
  call variant
contains

  subroutine original
    use mod
    type(poly), allocatable :: p
    class(func), allocatable :: f, df
    real :: array(2) = [1, 2]
    allocate(p)
    p%array = array
    call move_alloc(p, f)
    call alloc_deriv(f, df)
    if (.not.allocated(f)) stop 1
    select type (f)
      type is (poly)
        if (.not.allocated(f%array)) stop 2 ! SANITIZER: HEAP-USE-AFTER-FREE
        if (size(f%array) /= size(array)) stop 3
        if (any(f%array /= array)) stop 4   ! SEGFAULTS HERE WITHOUT SANITIZER
      class default
        stop 5
    end select
    deallocate (f)
    select type (df)                        ! Verify that df is OK
      type is (func_deriv)
      if (.not.allocated(df%f)) stop 6
      select type (f => df%f)
        type is (poly)
          if (.not.allocated(f%array)) stop 7
          if (size(f%array) /= size(array)) stop 8
          if (any(f%array /= array)) stop 9
        class default
          stop 10
        end select
      class default
        stop 11
    end select
    deallocate (df)
  end

  subroutine variant
    type :: base
      integer :: i = 1
    end type
    type, extends(base) :: t1
      integer, allocatable :: array(:)
    end type
    type(t1) :: x
    type :: t2
      class(base), allocatable :: b
    end type
    type(t2), allocatable :: y
    type :: t3
      type(t1), allocatable :: b
    end type
    type(t3), allocatable :: z
    type :: t4
      type(t1) :: b
    end type
    type(t4), allocatable :: z2

    x%array = [1,2]
    allocate(y, source = t2(x))             ! This was the original problem...
    if (any (x%array /= [1,2])) stop 12     ! ... x%array was overwritten
    allocate(z, source = t3(x))
    if (any (x%array /= [1,2])) stop 13     !    -ditto-
    allocate(z2, source = t4(x))
    if (any (x%array /= [1,2])) stop 14     ! Was already OK
    deallocate (x%array, y, z, z2%b%array)
  end
end ! Valgrind shows "     in use at exit: 0 bytes in 0 blocks"
