! { dg-do run }
! Check "double" allocations of allocatable components (PR 20541).
!
! Contributed by Erik Edelmann  <eedelmann@gcc.gnu.org>
!            and Paul Thomas  <pault@gcc.gnu.org>
!
program main

  implicit none

  type foo
     integer, dimension(:), allocatable :: array
  end type foo

  type(foo),allocatable,dimension(:) :: mol
  type(foo),pointer,dimension(:) :: molp
  integer :: i

  allocate (mol(1))
  allocate (mol(1), stat=i)
  !print *, i  ! /= 0
  if (i == 0) call abort()

  allocate (mol(1)%array(5))
  allocate (mol(1)%array(5),stat=i)
  !print *, i  ! /= 0
  if (i == 0) call abort()

  allocate (molp(1))
  allocate (molp(1), stat=i)
  !print *, i  ! == 0
  if (i /= 0) call abort()

  allocate (molp(1)%array(5))
  allocate (molp(1)%array(5),stat=i)
  !print *, i  ! /= 0
  if (i == 0) call abort()

end program main
