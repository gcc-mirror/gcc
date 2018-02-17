! { dg-do run }
! Test assignments of derived type with allocatable components (PR 20541).
!
! Contributed by Erik Edelmann  <eedelmann@gcc.gnu.org>
!            and Paul Thomas  <pault@gcc.gnu.org>
!
  type :: ivs
    character(1), allocatable :: chars(:)
  end type ivs

  type(ivs) :: a, b
  type(ivs) :: x(3), y(3)
  
  allocate(a%chars(5))
  a%chars = (/"h","e","l","l","o"/)

! An intrinsic assignment must deallocate the l-value and copy across
! the array from the r-value.
  b = a
  if (any (b%chars .ne. (/"h","e","l","l","o"/))) STOP 1
  if (allocated (a%chars) .eqv. .false.) STOP 2

! Scalar to array needs to copy the derived type, to its ultimate components,
! to each of the l-value elements.  */
  x = b
  x(2)%chars = (/"g","'","d","a","y"/)
  if (any (x(1)%chars .ne. (/"h","e","l","l","o"/))) STOP 3
  if (any (x(2)%chars .ne. (/"g","'","d","a","y"/))) STOP 4
  if (allocated (b%chars) .eqv. .false.) STOP 5
  deallocate (x(1)%chars, x(2)%chars, x(3)%chars)

! Array intrinsic assignments are like their scalar counterpart and
! must deallocate each element of the l-value and copy across the
! arrays from the r-value elements.
  allocate(x(1)%chars(5), x(2)%chars(5), x(3)%chars(5))
  x(1)%chars = (/"h","e","l","l","o"/)
  x(2)%chars = (/"g","'","d","a","y"/)
  x(3)%chars = (/"g","o","d","a","g"/)
  y(2:1:-1) = x(1:2)
  if (any (y(1)%chars .ne. (/"g","'","d","a","y"/))) STOP 6
  if (any (y(2)%chars .ne. (/"h","e","l","l","o"/))) STOP 7
  if (any (x(3)%chars .ne. (/"g","o","d","a","g"/))) STOP 8

! In the case of an assignment where there is a dependency, so that a
! temporary is necessary, each element must be copied to its
! destination after it has been deallocated.
  y(2:3) = y(1:2)
  if (any (y(1)%chars .ne. (/"g","'","d","a","y"/))) STOP 9
  if (any (y(2)%chars .ne. (/"g","'","d","a","y"/))) STOP 10
  if (any (y(3)%chars .ne. (/"h","e","l","l","o"/))) STOP 11

! An identity assignment must not do any deallocation....!
  y = y
  if (any (y(1)%chars .ne. (/"g","'","d","a","y"/))) STOP 12
  if (any (y(2)%chars .ne. (/"g","'","d","a","y"/))) STOP 13
  if (any (y(3)%chars .ne. (/"h","e","l","l","o"/))) STOP 14
end
