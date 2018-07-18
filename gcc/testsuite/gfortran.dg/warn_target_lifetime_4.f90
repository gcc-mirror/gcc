! { dg-do compile }
! { dg-options "-Wtarget-lifetime" }
!
! PR fortran/81770: [5/6/7 Regression] Bogus warning: Pointer in pointer assignment might outlive the pointer target
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m

   type t
      integer, allocatable :: l
   end type

contains

   subroutine sub(c_in, list)
      type(t), target, intent(in)  :: c_in
      integer, pointer, intent(out) :: list

      type(t), pointer :: container

      container => c_in

      list => container%l

   end subroutine

end
