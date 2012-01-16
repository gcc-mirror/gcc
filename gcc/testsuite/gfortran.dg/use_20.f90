! { dg-do compile }
!
! PR fortran/51809
!
! Contributed by Kacper Kowalik
!
module foo
   implicit none

   type foo_t
   contains
      procedure :: func_foo
   end type foo_t

contains

   subroutine func_foo(this)
      implicit none
      class(foo_t), intent(in) :: this
   end subroutine func_foo

end module foo

module bar
   use foo,   only: foo_t

   implicit none

   type, extends(foo_t) :: bar_t
   contains
      procedure :: func_bar
   end type bar_t

contains

   subroutine func_bar(this)
      use foo,    only: foo_t     ! <--- removing this line also fixes ICE
      implicit none
      class(bar_t), intent(in) :: this
   end subroutine func_bar

end module bar

module merry_ICE
   use foo,  only: foo_t   ! <------ change order to prevent ICE
   use bar,  only: bar_t   ! <------ change order to prevent ICE
end module merry_ICE

! { dg-final { cleanup-modules "foo bar merry_ice" } }
