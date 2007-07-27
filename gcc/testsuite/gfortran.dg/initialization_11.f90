! { dg-do run }
! PR fortran/32903
!
program test
  implicit none
  type data_type
    integer :: i=2
  end type data_type
  type(data_type) :: d
  d%i = 4
  call set(d)
  if(d%i /= 2) then
     print *, 'Expect: 2, got: ', d%i
     call abort()
  end if
contains
  subroutine set(x1)
    type(data_type),intent(out):: x1
  end subroutine set
end program test
