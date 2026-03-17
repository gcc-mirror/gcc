! { dg-do compile }
! PR fortran/102333
! Contributed by G. Steinmetz <gscfq@t-online.de>
!
! ICE in gfc_generate_function_code with procedure pointer returning
! unlimited polymorphic pointer.

module m
contains
  function f() result(res)
    class(*), pointer :: res
    res => null()
  end function
  subroutine s()
    procedure(f), pointer :: p
    class(*), pointer :: x
    p => f
    x => p()
  end subroutine
end module
