! { dg-do run }
! Tests the fix for PR68196
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
  type AA
    integer :: i
    procedure(foo), pointer :: funct
  end type
  class(AA), allocatable :: my_AA
  type(AA) :: res

  allocate (my_AA, source = AA (1, foo))

  res = my_AA%funct ()

  if (res%i .ne. 3) call abort
  if (.not.associated (res%funct)) call abort
  if (my_AA%i .ne. 4) call abort
  if (associated (my_AA%funct)) call abort

contains
  function foo(A)
    class(AA), allocatable :: A
    type(AA) foo

    if (.not.allocated (A)) then
      allocate (A, source = AA (2, foo))
    endif

    select type (A)
      type is (AA)
        foo = AA (3, foo)
        A = AA (4, NULL ())
    end select
  end function
end
