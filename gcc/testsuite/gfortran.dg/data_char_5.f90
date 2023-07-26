! { dg-do compile }
! PR fortran/99205 - Issues with non-constant character length 

subroutine sub ()
  integer :: ll = 4
  block
    character(ll) :: c(2)
    data c /'a', 'b'/     ! { dg-error "Non-constant character length" }
  end block
contains
  subroutine sub1 ()
    character(ll) :: d(2)
    data d /'a', 'b'/     ! { dg-error "Non-constant character length" }
  end subroutine sub1
end subroutine sub
