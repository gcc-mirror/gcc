! { dg-do run }
! Tests the fix for PR35959, in which the structure subpattern was declared static
! so that this test faied on the second recursive call.
!
! Contributed by Michaël Baudin <michael.baudin@gmail.com>
!
program testprog
  type :: t_type
    integer, dimension(:), allocatable :: chars
  end type t_type
  integer, save :: callnb = 0
  type(t_type) :: this
  allocate ( this % chars ( 4))
  if (.not.recursivefunc (this) .or. (callnb .ne. 10)) STOP 1
contains
  recursive function recursivefunc ( this ) result ( match )
    type(t_type), intent(in) :: this
    type(t_type) :: subpattern
    logical :: match
    callnb = callnb + 1
    match = (callnb == 10)
    if ((.NOT. allocated (this % chars)) .OR. match) return
    allocate ( subpattern % chars ( 4 ) )
    match = recursivefunc ( subpattern )
  end function recursivefunc
end program testprog
