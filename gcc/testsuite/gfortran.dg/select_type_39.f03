! { dg-do run }
!
! Tests the fix for PR67564 comment #9.
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
class(*), allocatable :: val(:)
call get_value (val)
select type (val)
type is (character(*))
  if (size (val) .ne. 2) STOP 1
  if (len(val) .ne. 3) STOP 2
  if (any (val .ne. ['foo','bar'])) STOP 3
end select
contains
  subroutine get_value (value)
    class(*), allocatable, intent(out) :: value(:)
    allocate(value, source=['foo','bar'])
  end subroutine
end
