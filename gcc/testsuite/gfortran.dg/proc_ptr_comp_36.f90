! { dg-do compile }
!
! PR 54107: [4.8 Regression] Memory hog with abstract interface
!
! Contributed by Arjen Markus <arjen.markus895@gmail.com>

  implicit none 
  type computation_method 
    character(len=40)                           :: name 
    procedure(compute_routine), pointer, nopass :: compute 
  end type 
  abstract interface 
    subroutine compute_routine( param_value, zfunc, probability ) 
      real, dimension(:), intent(in) :: param_value 
      procedure(compute_routine)     :: zfunc 
      real, intent(in)               :: probability 
    end subroutine 
  end interface 
end
