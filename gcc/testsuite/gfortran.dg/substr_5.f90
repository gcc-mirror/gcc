! { dg-do run }
!
  character(*), parameter  :: chrs = '-+.0123456789eEdD'
  character(*), parameter  :: expr = '-+.0123456789eEdD'
  integer :: i

  if (index(chrs(:), expr) /= 1) STOP 1
  if (index(chrs(14:), expr) /= 0) STOP 2
  if (index(chrs(:12), expr) /= 0) STOP 3
  if (index(chrs, expr(:)) /= 1) STOP 4
  if (index(chrs, expr(1:)) /= 1) STOP 5
  if (index(chrs, expr(:1)) /= 1) STOP 6

  if (foo(expr) /= 1) STOP 7
  if (foo(expr) /= 1) STOP 8
  if (foo(expr) /= 1) STOP 9
  if (foo(expr(:)) /= 1) STOP 10
  if (foo(expr(1:)) /= 1) STOP 11
  if (foo(expr(:1)) /= 1) STOP 12

  call bar(expr)

contains
  subroutine bar(expr)
    character(*), intent(in) :: expr
    character(*), parameter  :: chrs = '-+.0123456789eEdD'
    integer :: foo

    if (index(chrs(:), expr) /= 1) STOP 13
    if (index(chrs(14:), expr) /= 0) STOP 14
    if (index(chrs(:12), expr) /= 0) STOP 15
    if (index(chrs, expr(:)) /= 1) STOP 16
    if (index(chrs, expr(1:)) /= 1) STOP 17
    if (index(chrs, expr(:1)) /= 1) STOP 18
  end subroutine bar

  integer function foo(expr)
    character(*), intent(in) :: expr
    character(*), parameter  :: chrs = '-+.0123456789eEdD'

    foo = index(chrs, expr)
  end function foo

end
