! { dg-do run }
!
! Check the fix for PR34640 comments 1 and 3.
!
! This involves passing and returning pointer array components that
! point to components of arrays of derived types.
!
MODULE test
  IMPLICIT NONE
  TYPE :: my_type
    INTEGER :: value
    integer :: tag
  END TYPE
CONTAINS
  SUBROUTINE get_values(values, switch)
    INTEGER, POINTER :: values(:)
    integer :: switch
    TYPE(my_type), POINTER :: d(:)
    allocate (d, source = [my_type(1,101), my_type(2,102)])
    if (switch .eq. 1) then
      values => d(:)%value
      if (any (values .ne. [1,2])) print *, values(2)
    else
      values => d(:)%tag
      if (any (values .ne. [101,102])) STOP 1
    end if
  END SUBROUTINE

  function return_values(switch) result (values)
    INTEGER, POINTER :: values(:)
    integer :: switch
    TYPE(my_type), POINTER :: d(:)
    allocate (d, source = [my_type(1,101), my_type(2,102)])
    if (switch .eq. 1) then
      values => d(:)%value
      if (any (values .ne. [1,2])) STOP 2
    else
      values => d(:)%tag
      if (any (values([2,1]) .ne. [102,101])) STOP 3
    end if
  END function
END MODULE

  use test
  integer, pointer :: x(:)
  type :: your_type
    integer, pointer :: x(:)
  end type
  type(your_type) :: y

  call get_values (x, 1)
  if (any (x .ne. [1,2])) STOP 4
  call get_values (y%x, 2)
  if (any (y%x .ne. [101,102])) STOP 5

  x => return_values (2)
  if (any (x .ne. [101,102])) STOP 6
  y%x => return_values (1)
  if (any (y%x .ne. [1,2])) STOP 7
end
