! { dg-do run }
! { dg-options "-std=legacy" }
!
! Tests the fix for PR29389, in which the  statement function would not be
! recognised as PURE within a PURE procedure.

! Contributed by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>

  INTEGER :: st1, i = 99, a(4), q = 6
  st1 (i) = i * i * i 
  FORALL(i=1:4) a(i) = st1 (i) 
  FORALL(i=1:4) a(i) = u (a(i)) - a(i)** 2 
  if (any (a .ne. 0)) call abort ()
  if (i .ne. 99) call abort ()
contains
  pure integer function u (x)
    integer,intent(in) :: x
    st2 (i) = i * i
    u = st2(x)
  end function
end
