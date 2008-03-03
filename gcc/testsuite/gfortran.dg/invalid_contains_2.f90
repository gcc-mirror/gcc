! { dg-do compile } 
! PR18923 segfault after subroutine name confusion.
program foo
contains
  subroutine foo(i) ! { dg-error "conflicts with PROCEDURE" }
    integer :: i ! { dg-error "data declaration statement" }
    character(len=selected_int_kind(i)) :: c ! { dg-error "data declaration statement" }
  end subroutine ! { dg-error "Expecting END PROGRAM statement" }
end program foo
