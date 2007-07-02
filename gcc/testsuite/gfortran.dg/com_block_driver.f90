! { dg-do run }
module myComModule
  use, intrinsic :: iso_c_binding

  common /COM2/ R2, S2
  real(c_double) :: r2
  real(c_double) :: s2 
  bind(c) :: /COM2/

end module myComModule

module comBlockTests
  use, intrinsic :: iso_c_binding
  use myComModule

  implicit none

  common /COM/ R, S
  real(c_double) :: r
  real(c_double) :: s 
  bind(c) :: /COM/

  contains

  subroutine testTypes()
    implicit none
  end subroutine testTypes
end module comBlockTests

program comBlockDriver
  use comBlockTests
  
  call testTypes()
end program comBlockDriver
