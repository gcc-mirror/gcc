! { dg-do compile }
! PR fortran/67525
! Code contributed by Gerhard Steinmetz
!
real function f(x)
   select type (x)         ! { dg-error "shall be polymorphic" }
   end select
end function f

real function g(x)
   select type (x=>null()) ! { dg-error "shall be polymorphic" }
   end select
end function g

subroutine a(x)
   select type (x)         ! { dg-error "shall be polymorphic" }
   end select
end subroutine a
