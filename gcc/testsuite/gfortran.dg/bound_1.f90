! { dg-do run }
  implicit none

  type test_type
    integer, dimension(5) :: a
  end type test_type

  type (test_type), target :: tt(2)
  integer i

  i = ubound(tt(1)%a, 1)
  if (i/=5) call abort()
  i = lbound(tt(1)%a, 1)
  if (i/=1) call abort()

  i = ubound(tt, 1)
  if (i/=2) call abort()
  i = lbound(tt, 1)
  if (i/=1) call abort()
end
