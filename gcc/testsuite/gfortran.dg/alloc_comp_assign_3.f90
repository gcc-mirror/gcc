! { dg-do run }
! Test assignments of nested derived types with allocatable components(PR 20541).
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

  x(1) = a((/1,2,3,4/))
  x(2) = a((/1,2,3,4/)+10)

  y(1) = b((/x(1),x(2)/))
  y(2) = b((/x(2),x(1)/))

  y(2) = y(1)
  forall (j=1:2,k=1:4, y(1)%at(j)%i(k) .ne. y(2)%at(j)%i(k)) &
                             y(1)%at(j)%i(k) = 999
  if (any ((/((y(1)%at(j)%i(k), k=1,4),j=1,2)/) .eq. 999)) STOP 1


  z = y
  forall (i=1:2,j=1:2,k=1:4, z(i)%at(j)%i(k) .ne. y(i)%at(j)%i(k)) &
                             z(i)%at(j)%i(k) = 999
  if (any ((/(((z(i)%at(j)%i(k), k=1,4),j=1,2),i=1,2)/) .eq. 999)) STOP 2

end
