! { dg-do run }
! { dg-additional-sources implicit_pure_5.c }
! PR fortran/96018 - a wrongly marked implicit_pure
! function caused wrong code.
module wrapper
  use, intrinsic :: iso_c_binding, only : c_int
  implicit none
  integer(kind=c_int), bind(C) :: num_calls
contains

  integer function call_side_effect() result(ierr)
    call side_effect(ierr)
  end function call_side_effect

  integer function inner_3d(array) result(ierr)
    real, intent(in) :: array(:,:,:)
    integer dimensions(3)
    dimensions = shape(array)
    ierr = call_side_effect()
  end function inner_3d

  integer function inner_4d(array) result(ierr)
    real, intent(in) :: array(:,:,:,:)
    integer dimensions(4)
    dimensions = shape(array)
    ierr = call_side_effect()
  end function inner_4d

  subroutine write_3d()
    real :: array(1,1,1)
    integer ierr
    ierr = inner_3d(array)
    ierr = call_side_effect()
  end subroutine write_3d

  subroutine write_4d()
    real array(1,1,1,1)
    integer ierr
    ierr = inner_4d(array)
    ierr = call_side_effect()
  end subroutine write_4d

  subroutine side_effect(ierr)
    integer, intent(out) :: ierr        ! Error code
    interface
       integer(c_int) function side_effect_c() bind(C,name='side_effect_c')
         use, intrinsic :: iso_c_binding, only: c_int
       end function side_effect_c
    end interface
    ierr = side_effect_c()
  end subroutine side_effect

end module wrapper

program self_contained
  use wrapper
  implicit none
  call write_3d()
  if (num_calls /= 2) stop 1
  call write_4d()
  if (num_calls /= 4) stop 2
end program self_contained

