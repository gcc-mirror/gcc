! { dg-do compile }
! PR fortran/36803
! PR fortran/36795
!
! "(n)" was simplified to the EXPR_VARIABLE "n"
! and thus "(n)" was judged as definable.
!
interface
  subroutine foo(x)
    character, intent(out) :: x(:)  ! or INTENT(INOUT)
  end subroutine foo
end interface
character :: n(5)
call foo( (n) ) ! { dg-error "Non-variable expression" }
end
