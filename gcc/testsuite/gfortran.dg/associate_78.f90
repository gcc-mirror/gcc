! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR123352, which failed as shown. The operator in the first
! selector was not being resolved and so 'op_foo' did not have a type.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module tensors_m
  implicit none

  type foo_t
  contains
    generic :: operator(.op.) => op
    procedure op
    procedure f
  end type

contains

  type(foo_t) function op(self)
    class(foo_t), intent(in) :: self
    op = self
  end function

  integer function f(self)
    class(foo_t) self
    f = 42
  end function

end module

  use tensors_m
  implicit none
  type(foo_t) foo

  associate(op_foo => .op. foo)
    associate(op_foo_f => op_foo%f()) ! Error: Invalid association target at (1)
      print *, op_foo_f
    end associate
  end associate                       ! Error: Expecting END PROGRAM statement at (1)
end
! { dg-final { scan-tree-dump-times "struct foo_t op_foo;" 1 "original" } }
! { dg-final { scan-tree-dump-times "integer.kind=4. op_foo_f;" 1 "original" } }
