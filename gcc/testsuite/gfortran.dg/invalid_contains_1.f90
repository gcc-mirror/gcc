! { dg-do compile } 
! PR18923 segfault after subroutine name confusion.
module FOO
contains
  subroutine FOO ! { dg-error "conflicts with PROCEDURE" }
    character(len=selected_int_kind(0)) :: C ! { dg-error "data declaration statement" }
  end subroutine ! { dg-error "Expecting END MODULE statement" }
end ! { dg-warning "CONTAINS statement without FUNCTION" }