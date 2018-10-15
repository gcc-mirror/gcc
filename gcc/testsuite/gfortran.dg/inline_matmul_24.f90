! { dg-do run }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }
!
! PR fortran/87597
!
! Contributed by gallmeister
!
! Before, for the inlined matmul,
! gamma5 was converted to an EXPR_ARRAY with lbound = 1
! instead of the lbound = 0 as declared; leading to
! an off-by-one problem.
!
program testMATMUL
  implicit none
    complex, dimension(0:3,0:3), parameter :: gamma5 = reshape((/ 0., 0., 1., 0., &
                                                                  0., 0., 0., 1., &
                                                                  1., 0., 0., 0., &
                                                                  0., 1., 0., 0. /),(/4,4/))
    complex, dimension(0:3,0:3) :: A, B, D
    integer :: i

    A = 0.0
    do i=0,3
       A(i,i) = i*1.0
    end do

    B = cmplx(7,-9)
    B = matmul(A,gamma5)

    D = reshape([0, 0, 2, 0, &
                 0, 0, 0, 3, &
                 0, 0, 0, 0, &
                 0, 1, 0, 0], [4, 4])
    write(*,*) B(0,:)
    write(*,*) B(1,:)
    write(*,*) B(2,:)
    write(*,*) B(3,:)
    if (any(B /= D)) then
      call abort()
    end if
end program testMATMUL
! { dg-final { scan-tree-dump-times "gamma5\\\[__var_1_do \\* 4 \\+ __var_2_do\\\]|gamma5\\\[NON_LVALUE_EXPR <__var_1_do> \\* 4 \\+ NON_LVALUE_EXPR <__var_2_do>\\\]" 1 "original" } }
