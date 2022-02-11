! { dg-do compile }
! PR fortran/103411 - ICE in gfc_conv_array_initializer
! Based on testcase by G. Steinmetz
! Test simplifications for checks of shape argument to reshape intrinsic

program p
  integer :: i
  integer, parameter :: a(2) = [2,2]
  integer, parameter :: u(5) = [1,2,2,42,2]
  integer, parameter :: v(1,3) = 2
  integer, parameter :: d(2,2) = reshape([1,2,3,4,5], a)
  integer, parameter :: c(2,2) = reshape([1,2,3,4], a)
  integer, parameter :: b(2,2) = &
           reshape([1,2,3], a) ! { dg-error "not enough elements" }
  print *, reshape([1,2,3], a) ! { dg-error "not enough elements" }
  print *, reshape([1,2,3,4], a)
  print *, reshape([1,2,3,4,5], a)
  print *, b, c, d
  print *, reshape([1,2,3], [(u(i),i=1,2)])
  print *, reshape([1,2,3], [(u(i),i=2,3)]) ! { dg-error "not enough elements" }
  print *, reshape([1,2,3],              &
                   [(u(i)*(-1)**i,i=2,3)]) ! { dg-error "has negative element" }
  print *, reshape([1,2,3,4], u(5:3:-2))
  print *, reshape([1,2,3],   u(5:3:-2))  ! { dg-error "not enough elements" }
  print *, reshape([1,2,3,4], u([5,3]))
  print *, reshape([1,2,3]  , u([5,3]))   ! { dg-error "not enough elements" }
  print *, reshape([1,2,3,4], v(1,2:))
  print *, reshape([1,2,3],   v(1,2:))    ! { dg-error "not enough elements" }
  print *, reshape([1,2,3,4], v(1,[2,1]))
  print *, reshape([1,2,3] ,  v(1,[2,1])) ! { dg-error "not enough elements" }
end
