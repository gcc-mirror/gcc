! { dg-do compile }
!
! PR 41733: Proc-pointer conformance checks: Elemental-proc-ptr => non-elemental-procedure
!
! Contributed by James Van Buskirk

module funcs
   implicit none
   abstract interface
      real elemental function fun(x)
         real, intent(in) :: x
      end function
   end interface
contains
  function my_dcos(x)
    real, intent(in) :: x
    real :: my_dcos
    my_dcos = cos(x)
  end function
end module

program start
   use funcs
   implicit none
   procedure(fun), pointer :: f
   real x(3)
   x = [1,2,3]
   f => my_dcos     ! { dg-error "Mismatch in PURE attribute" }
   write(*,*) f(x)
end program start 

! { dg-final { cleanup-modules "funcs" } }