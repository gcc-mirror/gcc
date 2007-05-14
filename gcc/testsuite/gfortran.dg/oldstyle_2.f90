! { dg-do compile }
subroutine foo(i) ! { dg-error "DATA attribute" }
  integer i /10/
end subroutine foo
