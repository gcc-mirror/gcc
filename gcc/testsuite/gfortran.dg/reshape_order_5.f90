! { dg-do compile }
!
! PR fortran/37203 - check RESHAPE arguments
!

  integer, dimension(6) :: source1 = (/ 1, 2, 3, 4, 5, 6 /)
  integer, dimension(2) :: shape1 = (/ 2, 5/)
  integer, dimension(2) :: pad1 = (/ 0, 0/)
  integer, dimension(2) :: t(2,5)

  t = reshape(source1, shape1, pad1, (/2, 1/))        ! ok
  t = reshape(source1, shape1, pad1, (/2.1, 1.2/))    ! { dg-error "must be INTEGER" }
  t = reshape(source1, shape1, pad1, (/2, 2/))        ! { dg-error "invalid permutation" }
  t = reshape(source1, shape1, pad1, (/2, 3/))        ! { dg-error "out-of-range dimension" }
  t = reshape(source1, shape1, pad1, (/2/))           ! { dg-error "wrong number of elements" }
end
