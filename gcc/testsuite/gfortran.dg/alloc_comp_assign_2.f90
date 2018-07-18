! { dg-do run }
! Test FORALL and WHERE with derived types with allocatable components (PR 20541).
!
! Contributed by Erik Edelmann  <eedelmann@gcc.gnu.org>
!            and Paul Thomas  <pault@gcc.gnu.org>
!
  type :: a
    integer, allocatable :: i(:)
  end type a

  type :: b
    type (a), allocatable :: at(:)
  end type b

  type(a) :: x(2)
  type(b) :: y(2), z(2)
  integer i, m(4)

! Start with scalar and array element assignments in FORALL.

  x(1) = a ((/1, 2, 3, 4/))
  x(2) = a ((/1, 2, 3, 4/) + 10)
  forall (j = 1:2, i = 1:4, x(j)%i(i) > 2 + (j-1)*10)  x(j)%i(i) =  j*4-i
  if (any ((/((x(i)%i(j), j = 1,4), i = 1,2)/) .ne. &
          (/1, 2, 1, 0, 11, 12, 5, 4/))) STOP 1

  y(1) = b ((/x(1),x(2)/))
  y(2) = b ((/x(2),x(1)/))
  forall (k = 1:2, j=1:2, i = 1:4, y(k)%at(j)%i(i) <= 10)
    y(k)%at(j)%i(i) =  j*4-i+k
  end forall
  if (any ((/(((y(k)%at(i)%i(j), j = 1,4), i = 1,2), k = 1,2)/) .ne. &
         (/4,3,2,1,11,12,6,5,11,12,3,2,9,8,7,6/))) STOP 2 

! Now simple assignments in WHERE.

  where (y(1)%at(1)%i > 2) y(1)%at(1)%i = 0
  if (any( (/(((y(k)%at(i)%i(j), j = 1,4), i = 1,2), k = 1,2)/) .ne. &
         (/0,0,2,1,11,12,6,5,11,12,3,2,9,8,7,6/))) STOP 3 

! Check that temporaries and full array  alloctable component assignments
! are correctly handled in FORALL.

  x = (/a ((/1,2,3,4/)),a ((/5,6,7,8/))/)
  forall (i=1:2) y(i) = b ((/x(i)/))
  forall (i=1:2) y(i) = y(3-i)      ! This needs a temporary.
  forall (i=1:2) z(i) = y(i)
  if (any ((/(((z(k)%at(i)%i(j), j = 1,4), i = 1,1), k = 1,2)/) .ne. &
         (/(/5,6,7,8/),(/1,2,3,4/)/))) STOP 4

end
