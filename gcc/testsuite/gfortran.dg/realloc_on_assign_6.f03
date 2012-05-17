! { dg-do compile }
! Test the fix for PR48456 and PR48360 in which the backend
! declarations for components were not located in the automatic
! reallocation on assignments, thereby causing ICEs.
!
! Contributed by Keith Refson  <krefson@googlemail.com>
! and Douglas Foulds  <mixnmaster@gmail.com>
!
! This is PR48360

module m
  type mm
     real, dimension(3,3) :: h0
  end type mm
end module m

module gf33

  real, allocatable, save, dimension(:,:) :: hmat
  
contains
  subroutine assignit
    
    use m
    implicit none
    
    type(mm) :: mmv
    
    hmat = mmv%h0
  end subroutine assignit
end module gf33

! This is PR48456

module custom_type

integer, parameter :: dp = kind(0.d0)

type :: my_type_sub
    real(dp), dimension(5) :: some_vector
end type my_type_sub

type :: my_type
  type(my_type_sub) :: some_element
end type my_type

end module custom_type

module custom_interfaces

interface
  subroutine store_data_subroutine(vec_size)
  implicit none
  integer, intent(in) :: vec_size
  integer :: k
  end subroutine store_data_subroutine
end interface

end module custom_interfaces

module store_data_test

use custom_type

save
type(my_type), dimension(:), allocatable :: some_type_to_save

end module store_data_test

program test

use store_data_test

integer :: vec_size

vec_size = 2

call store_data_subroutine(vec_size)
call print_after_transfer()

end program test

subroutine store_data_subroutine(vec_size)

use custom_type
use store_data_test

implicit none

integer, intent(in) :: vec_size
integer :: k

allocate(some_type_to_save(vec_size))

do k = 1,vec_size

  some_type_to_save(k)%some_element%some_vector(1) = 1.0_dp
  some_type_to_save(k)%some_element%some_vector(2) = 2.0_dp
  some_type_to_save(k)%some_element%some_vector(3) = 3.0_dp
  some_type_to_save(k)%some_element%some_vector(4) = 4.0_dp
  some_type_to_save(k)%some_element%some_vector(5) = 5.0_dp

end do

end subroutine store_data_subroutine

subroutine print_after_transfer()

use custom_type
use store_data_test

implicit none

real(dp), dimension(:), allocatable :: C_vec
integer :: k

allocate(C_vec(5))

do k = 1,size(some_type_to_save)

  C_vec = some_type_to_save(k)%some_element%some_vector
  print *, "C_vec", C_vec

end do

end subroutine print_after_transfer
