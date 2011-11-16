! { dg-do compile }
!
! PR fortran/39427
!
! Check constructor functionality.
!
! Contributed by Damian Rouson.
!
module mycomplex_module
   private
   public :: mycomplex
   type mycomplex
!      private
      real :: argument, modulus
   end type
   interface mycomplex
      module procedure complex_to_mycomplex, two_reals_to_mycomplex
   end interface
!   :
   contains
      type(mycomplex) function complex_to_mycomplex(c)
         complex, intent(in) :: c
!         :
      end function complex_to_mycomplex
      type(mycomplex) function two_reals_to_mycomplex(x,y)
         real, intent(in)           :: x
         real, intent(in), optional :: y
!         :
       end function two_reals_to_mycomplex
!       :
    end module mycomplex_module
!    :
program myuse
    use mycomplex_module
    type(mycomplex) :: a, b, c
!    :
    a = mycomplex(argument=5.6, modulus=1.0)  ! The structure constructor
    c = mycomplex(x=0.0, y=1.0)               ! A function reference
    c = mycomplex(0.0, 1.0)               ! A function reference
end program myuse

! { dg-final { cleanup-modules "mycomplex_module" } }
