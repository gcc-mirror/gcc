! { dg-do compile }
!
subroutine foo(that)
  implicit none
  class(*),  target, intent(in)  :: this
  class(*), pointer, intent(out) :: that

  that => this
  return
end subroutine foo
! { dg-error "Symbol at \\\(1\\\) is not a DUMMY variable" "" { target "*-*-*" } 5 }
