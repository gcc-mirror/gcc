! { dg-do compile }
subroutine foo(a)
  dimension  a(*,*) ! { dg-error "Bad specification for assumed size array" }
end
