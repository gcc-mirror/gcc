! { dg-do compile }
! PR 51502 - this was wrongly detected to be implicit pure.
module m
  integer :: i
contains
  subroutine foo(x)
    integer, intent(inout) :: x
    outer: block
      block
        i = 5
      end block
    end block outer
  end subroutine foo
end module m

! { dg-final { scan-module-absence "m" "IMPLICIT_PURE" } }
