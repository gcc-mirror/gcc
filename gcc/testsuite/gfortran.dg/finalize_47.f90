! { dg-do run }
!
! Check that PR91316 is fixed. Note removal of recursive I/O.
!
! Contributed by Jose Rui Faustino de Sousa  <jrfsousa@gcc.gnu.org>
!
! NAGFOR complains correctly about the finalization of an INTENT(OUT) dummy
! with an impure finalization subroutine, within a pure procedure.
! It also complains about the finalization of final_set, which does not seem
! to be correct (see finalize_50.f90).
! Both procedures have been made impure so that this testcase runs with both
! compilers.
!
module final_m
  implicit none
  private
  public ::        &
    assignment(=)

  public :: &
    final_t

  public ::     &
    final_init, &
    final_set,  &
    final_get,  &
    final_end

  type :: final_t
    private
    integer :: n = -1
  contains
    final :: final_end
  end type final_t

  interface assignment(=)
    module procedure final_init
  end interface assignment(=)

  integer, public :: final_ctr = 0
  integer, public :: final_res = 0

contains

  impure elemental subroutine final_init(this, n)
    type(final_t), intent(out) :: this
    integer,       intent(in)  :: n
    this%n = n
  end subroutine final_init

  impure elemental function final_set(n) result(this)
    integer, intent(in) :: n
    type(final_t) :: this
    this%n = n
  end function final_set

  elemental function final_get(this) result(n)
    type(final_t), intent(in) :: this
    integer :: n
    n = this%n
  end function final_get

  subroutine final_end(this)
    type(final_t), intent(inout) :: this
!    print *, "DESTROY: ", this%n !< generates illegal, recursive io in 'final_s4'
    final_res = this%n
    final_ctr = final_ctr + 1
    this%n = -1
  end subroutine final_end
end module final_m

program final_p
  use final_m
  implicit none
  type(final_t) :: f0
!  call final_init(f0, 0)
  call final_s1()
  call final_s2()
  call final_s3()
  call final_s4()
  call final_end(f0)
contains
  subroutine final_s1()
    type(final_t) :: f
    call final_init(f, 1)
    print *, "f1: ", final_get(f)
    if ((final_ctr .ne. 1) .or. (final_res .ne. -1)) stop 1
  end subroutine final_s1
  subroutine final_s2()
    type(final_t) :: f
    f = 2
    print *, "f2: ", final_get(f)
    if ((final_ctr .ne. 3) .or. (final_res .ne. -1)) stop 1
  end subroutine final_s2
  subroutine final_s3()
    type(final_t) :: f
    f = final_set(3)
    print *, "f3: ", final_get(f)
    if ((final_ctr .ne. 6) .or. (final_res .ne. 3)) stop 1
  end subroutine final_s3
  subroutine final_s4()
    print *, "f4: ", final_get(final_set(4))
    if ((final_ctr .ne. 8) .or. (final_res .ne. 4)) stop 1
  end subroutine final_s4
end program final_p
