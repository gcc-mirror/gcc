! { dg-do run }
! F2023 7.5.4.1: a component may itself be of parameterized derived type,
! with its parameters given by expressions in the outer parameters.
! This is ~/pos/p22_E_nested_pdt.f90 in the attachment to comment #10
! of PR82173.
module m_p22
  implicit none
  type :: leaf(k, n)
    integer, kind :: k = 4
    integer, len :: n = 1
    real(k) :: v(n)
    character(len=n) :: s
  end type
  type :: branch(k, n)
    integer, kind :: k = 4
    integer, len :: n = 1
    type(leaf(k, n)) :: same
    type(leaf(k, 2*n)) :: twice
    type(leaf(8, 3)) :: fixed
    integer :: tag(n)
  end type
end module

program p22
  use m_p22
  implicit none
  type(branch(4, 3)) :: b

  b%same%v = 1.0_4
  b%same%s = 'abc'
  b%twice%v = 2.0_4
  b%twice%s = 'abcdef'
  b%fixed%v = 3.0_8
  b%fixed%s = 'xyz'
  b%tag = 7

  if (b%k /= 4 .or. b%n /= 3) error stop 1
  if (b%same%k /= 4 .or. b%same%n /= 3) error stop 2
  if (b%twice%n /= 6) error stop 3
  if (b%fixed%k /= 8 .or. b%fixed%n /= 3) error stop 4

  if (size (b%same%v) /= 3) error stop 5
  if (size (b%twice%v) /= 6) error stop 6
  if (kind (b%fixed%v) /= 8) error stop 7
  if (len (b%same%s) /= 3 .or. len (b%twice%s) /= 6) error stop 8

  if (abs (sum (b%same%v) - 3.0_4) > 1.0e-6_4) error stop 9
  if (abs (sum (b%twice%v) - 12.0_4) > 1.0e-6_4) error stop 10
  if (b%same%s /= 'abc' .or. b%twice%s /= 'abcdef') error stop 11
  if (sum (b%tag) /= 21) error stop 12
end program
