! { dg-do run }
! { dg-options "-ffrontend-optimize -fdump-tree-optimized -Wrealloc-lhs -finline-matmul-limit=1000 -O" }
! PR 66094: Check functionality for MATMUL(TRANSPOSE(A),B)) for two-dimensional arrays
program main
  implicit none
  integer :: in, im, icnt
  integer, volatile :: ten

  ten = 10
  ! cycle through a few test cases...
  do in = 2,ten 
     do im = 2,ten
        do icnt = 2,ten
           block
             real, dimension(icnt,in) :: a2
             real, dimension(icnt,im) :: b2
             real, dimension(in,im) :: c2,cr
             integer :: i,j,k
             call random_number(a2)
             call random_number(b2)
             c2 = 0
             do i=1,size(a2,2)
                do j=1, size(b2,2)
                   do k=1, size(a2,1)
                      c2(i,j) = c2(i,j) + a2(k,i) * b2(k,j)
                   end do
                end do
             end do
             cr = matmul(transpose(a2), b2)
             if (any(abs(c2-cr) > 1e-4)) STOP 7
           end block
        end do
     end do
  end do
end program main
! { dg-final { scan-tree-dump-times "_gfortran_matmul" 1 "optimized" } }
