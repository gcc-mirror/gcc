! { dg-do compile }
! { dg-options "-fdump-tree-original -fcheck=pointer" }
!
! Test the fix for PR82376. The pointer check was doubling up the call
! to new. The fix reduces the count of 'new' from 5 to 4, or to 3, when
! counting only calls.
!
! Contributed by José Rui Faustino de Sousa  <jrfsousa@gmail.com>
!
program main_p

  integer, parameter :: n = 10

  type :: foo_t
    integer, pointer :: v =>null()
  end type foo_t

  integer, save :: pcnt = 0

  type(foo_t) :: int
  integer     :: i

  do i = 1, n
    call init(int, i)
    if(.not.associated(int%v)) stop 1
    if(int%v/=i) stop 2
    if(pcnt/=i) stop 3
  end do

contains

  function new(data) result(this)
    integer, target, intent(in) :: data

    integer, pointer :: this

    nullify(this)
    this => data
    pcnt = pcnt + 1
    return
  end function new

  subroutine init(this, data)
    type(foo_t), intent(out) :: this
    integer,     intent(in)  :: data

    call set(this, new(data))
    return
  end subroutine init

  subroutine set(this, that)
    type(foo_t),     intent(inout) :: this
    integer, target, intent(in)    :: that

    this%v => that
    return
  end subroutine set

end program main_p
! { dg-final { scan-tree-dump-times { new \(} 3 "original" } }
