! { dg-do compile }
!
! PR 78592: [7 Regression] ICE in gfc_find_specific_dtio_proc, at fortran/interface.c:4939
!
! Contributed by Mikael Morin <morin-mikael@orange.fr>

program p
   type t
   end type
   type(t) :: z
   type, extends(t) :: t2
   end type
   class(t2), allocatable :: z2
   interface write(formatted)
      procedure wf2
      module procedure wf   ! { dg-error "is neither function nor subroutine" }
   end interface
   print *, z
   allocate(z2)
   print *, z2
  contains
   subroutine wf2(this, a, b, c, d, e)  ! { dg-error "must have assumed length" }
      class(t2), intent(in) :: this
      integer, intent(in) :: a
      character(*), intent(in) :: b
      integer, intent(in) :: c(:)
      integer, intent(out) :: d
      character, intent(inout) :: e
   end subroutine wf2
end
