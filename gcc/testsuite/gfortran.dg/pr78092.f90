! { dg-do run }
program test_stuff

  implicit none

  integer :: ivar1(2,3), ivar2
  
  ivar1 = 6
  call poly_sizeof(ivar1, ivar2)

  if (ivar2 /= 4) call abort

  contains
  
  subroutine poly_sizeof(arg1,arg2)
    class(*), intent(in) :: arg1(:,:)
    integer, intent(out) :: arg2
    arg2 = sizeof(arg1(1,1))
  end subroutine

end program test_stuff
