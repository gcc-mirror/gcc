! { dg-do compile }
elemental subroutine sub2(x)
   integer, intent(in) :: x
   entry sub2_c(x) bind(c)    ! { dg-error "prohibited in an elemental" }
end subroutine sub2

elemental function func2(x)
   integer, intent(in) :: x
   entry func2_c(x) bind(c)   ! { dg-error "prohibited in an elemental" }
end function func2
