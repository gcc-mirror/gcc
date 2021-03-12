! { dg-do compile }
! PR fortran/99205 - Issues with non-constant character length 

subroutine sub ()
  integer :: ll = 4
  block
    character(ll) :: c(2) ! { dg-error "non-constant" }
    data c /'a', 'b'/
  end block
contains
  subroutine sub1 ()
    character(ll) :: d(2) ! { dg-error "non-constant" }
    data d /'a', 'b'/
  end subroutine sub1
end subroutine sub
