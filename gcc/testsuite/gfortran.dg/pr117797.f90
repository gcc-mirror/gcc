! { dg-do run }
!
! Test the fix for the regression caused by r15-5083.
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
module foo

  type, public :: any_matrix
    private
    class(*), allocatable :: value(:,:)
  end type

contains

  function bar(this) result(uptr)
    class(any_matrix), target, intent(in) :: this
    class(*), pointer :: uptr(:,:)
    uptr => this%value ! Seg. fault in trans-array.cc(gfc_get_array_span) here
  end function

  function build(this) result (res)
    class(*) :: this(:,:)
    type(any_matrix) :: res
    res%value = this
  end function

  function evaluate (this) result (res)
    class(*) :: this(:,:)
    character(len = 2, kind = 1), allocatable :: res(:)
      select type (ans => this)
        type is (character(*))
          res = reshape (ans, [4])
        type is (integer)
          allocate (res (8))
          write (res, '(i2)') ans
        class default
          res = ['no','t ','OK','!!']
      end select
  end

end module

  use foo
  class(*), allocatable :: up (:, :)
  character(len = 2, kind = 1) :: chr(2,2) = reshape (['ab','cd','ef','gh'], [2,2])
  integer :: i(2,2) = reshape ([1,2,3,4], [2,2])
  up = bar (build (chr))
  if (any (evaluate (up) /= reshape (chr, [4]))) stop 1

  up = bar (build (i))
  if (any (evaluate (up) /= [' 1',' 2',' 3',' 4'])) stop 2

  deallocate (up)
end
