! { dg-do run }
! { dg-set-target-env-var GFORTRAN_NUM_IMAGES "4" }
! { dg-options "-fdump-tree-original" }

program main
  call test(.true.)
  call test(.false.)
contains
  subroutine test(flag)
    logical, intent(in) :: flag
    integer, save, dimension(:), allocatable :: a[:]
    if (flag) then
       allocate (a(4)[*])
       a = this_image()
    else
       if (size(a,1) /= 4) stop 1
       if (any(a /= this_image())) stop 2
    end if
  end subroutine test
end program main
