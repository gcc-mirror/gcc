! { dg-do compile }

! Check PR84244 does not ICE anymore.

program ptr_comp_5
  integer, target :: dest = 42
  type t
    integer, pointer :: p
  end type
  type(t) :: o[*]

  o%p => dest
contains
  ! This unused routine is crucial for the ICE.
  function f(x)
    type(t), intent(in) ::x
  end function
end program

