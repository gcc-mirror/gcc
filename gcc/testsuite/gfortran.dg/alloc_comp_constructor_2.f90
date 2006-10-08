! { dg-do run }
! Test constructors of nested derived types with allocatable components(PR 20541).
!
! Contributed by Erik Edelmann  <eedelmann@gcc.gnu.org>
!            and Paul Thomas  <pault@gcc.gnu.org>
!
  type :: thytype
    integer(4), allocatable :: h(:)
  end type thytype

  type :: mytype
    type(thytype), allocatable :: q(:)
  end type mytype

  type (mytype) :: x
  type (thytype) :: w(2)
  integer :: y(2) =(/1,2/)

  w = (/thytype(y), thytype (2*y)/)
  x = mytype (w)
  if (any ((/((x%q(j)%h(i),j=1,2),i=1,2)/) .ne. (/1,2,2,4/))) call abort ()

  x = mytype ((/thytype(3*y), thytype (4*y)/))
  if (any ((/((x%q(j)%h(i),j=1,2),i=1,2)/) .ne. (/3,4,6,8/))) call abort ()

end
