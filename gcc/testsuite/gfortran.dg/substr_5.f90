! { dg-do run }
!
  character(*), parameter  :: chrs = '-+.0123456789eEdD'
  character(*), parameter  :: expr = '-+.0123456789eEdD'
  integer :: i

  if (index(chrs(:), expr) /= 1) call abort
  if (index(chrs(14:), expr) /= 0) call abort
  if (index(chrs(:12), expr) /= 0) call abort
  if (index(chrs, expr(:)) /= 1) call abort
  if (index(chrs, expr(1:)) /= 1) call abort
  if (index(chrs, expr(:1)) /= 1) call abort

  if (foo(expr) /= 1) call abort
  if (foo(expr) /= 1) call abort
  if (foo(expr) /= 1) call abort
  if (foo(expr(:)) /= 1) call abort
  if (foo(expr(1:)) /= 1) call abort
  if (foo(expr(:1)) /= 1) call abort

  call bar(expr)

contains
  subroutine bar(expr)
    character(*), intent(in) :: expr
    character(*), parameter  :: chrs = '-+.0123456789eEdD'
    integer :: foo

    if (index(chrs(:), expr) /= 1) call abort
    if (index(chrs(14:), expr) /= 0) call abort
    if (index(chrs(:12), expr) /= 0) call abort
    if (index(chrs, expr(:)) /= 1) call abort
    if (index(chrs, expr(1:)) /= 1) call abort
    if (index(chrs, expr(:1)) /= 1) call abort
  end subroutine bar

  integer function foo(expr)
    character(*), intent(in) :: expr
    character(*), parameter  :: chrs = '-+.0123456789eEdD'

    foo = index(chrs, expr)
  end function foo

end
