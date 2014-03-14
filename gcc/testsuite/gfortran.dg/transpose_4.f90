! { dg-do run }
!
! PR fortran/60392
! In the transposed case call to my_mul_cont, the compiler used to (wrongly)
! reuse a transposed descriptor for an array that was not transposed as a result
! of packing.
!
! Original test case from Alexander Vogt <a.vogt@fulguritus.com>.

program test
  implicit none

  integer, dimension(2,2) :: A, R, RT
  integer, dimension(2,2) :: B1, B2

  ! 
  ! A = [  2   17 ]
  !     [ 82  257 ]
  !
  ! matmul(a,a) = [  1398   4403 ]
  !               [ 21238  67443 ]
  !
  ! matmul(transpose(a), a) = [  6728  21108 ]
  !                           [ 21108  66338 ]
  A(1,1) =   2
  A(1,2) =  17
  A(2,1) =  82
  A(2,2) = 257

  R(1,1) =  1398
  R(1,2) =  4403
  R(2,1) = 21238
  R(2,2) = 67443
  
  RT(1,1) =  6728
  RT(1,2) = 21108
  RT(2,1) = 21108
  RT(2,2) = 66338

  ! Normal argument
  B1 = 0
  B2 = 0
  B1 = my_mul(A,A)
  B2 = my_mul_cont(A,A)
! print *,'Normal:    ',maxval(abs(B1-B2))
! print *,B1
! print *,B2
  if (any(B1 /= R)) call abort
  if (any(B2 /= R)) call abort

  ! Transposed argument
  B1 = 0
  B2 = 0
  B1 = my_mul(transpose(A),A)
  B2 = my_mul_cont(transpose(A),A)
! print *,'Transposed:',maxval(abs(B1-B2))
! print *,B1
! print *,B2
  if (any(B1 /= RT)) call abort
  if (any(B2 /= RT)) call abort

contains

  function my_mul(A,C) result (B)
    use, intrinsic :: ISO_Fortran_env
    integer, intent(in) :: A(2,2), C(2,2)
    integer :: B(2,2)
    B = matmul(A, C)
  end function

  function my_mul_cont(A,C) result (B)
    use, intrinsic :: ISO_Fortran_env
    integer, intent(in), contiguous :: A(:,:), C(:,:)
    integer :: B(2,2)
    B = matmul(A, C)
  end function

end program
