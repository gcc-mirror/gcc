! { dg-do compile }
! Tests the fix for the second bit of PR29389, in which the
! statement function would not be recognised as not PURE
! when it referenced a procedure that is not PURE.
!
! This is based on stfunc_4.f90 with the statement function made
! impure by a reference to 'v'.
!
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
    st2 (i) = i * v(i) ! { dg-error "non-PURE procedure" }
    u = st2(x)
  end function
  integer function v (x)
    integer,intent(in) :: x
    v = i
  end function
end
