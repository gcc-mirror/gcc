! Program to test arrays
! The program outputs a series of numbers.
! Two digit numbers beginning with 0, 1, 2 or 3 is a normal.
! Three digit numbers starting with 4 indicate an error.
! Using 1D arrays isn't a sufficient test, the first dimension is often
! handled specially.

! Fixed size parameter
subroutine f1 (a)
   implicit none
   integer, dimension (5, 8) :: a

   if (a(1, 1) .ne. 42) call abort

   if (a(5, 8) .ne. 43) call abort
end subroutine


program testprog
   implicit none
   integer, dimension(3:7, 4:11) :: a
   a(:,:) = 0
   a(3, 4) = 42
   a(7, 11) = 43
   call test(a)
contains
subroutine test (parm)
   implicit none
   ! parameter
   integer, dimension(2:, 3:) :: parm
   ! Known size arry
   integer, dimension(5, 8) :: a
   ! Known size array with different bounds
   integer, dimension(4:8, 3:10) :: b
   ! Unknown size arrays
   integer, dimension(:, :), allocatable :: c, d, e
   ! Vectors
   integer, dimension(5) :: v1
   integer, dimension(10, 10) :: v2
   integer n
   external f1

   ! Same size
   allocate (c(5,8))
   ! Same size, different bounds
   allocate (d(11:15, 12:19))
   ! A larger array
   allocate (e(15, 24))
   a(:,:) = 0
   b(:,:) = 0
   c(:,:) = 0
   d(:,:) = 0
   a(1,1) = 42
   b(4, 3) = 42
   c(1,1) = 42
   d(11,12) = 42
   a(5, 8) = 43
   b(8, 10) = 43
   c(5, 8) = 43
   d(15, 19) = 43

   v2(:, :) = 0
   do n=1,5
     v1(n) = n
   end do

   v2 (3, 1::2) = v1 (5:1:-1)
   v1 = v1 + 1

   if (v1(1) .ne. 2) call abort
   if (v2(3, 3) .ne. 4) call abort

   ! Passing whole arrays
   call f1 (a)
   call f1 (b)
   call f1 (c)
   call f2 (a)
   call f2 (b)
   call f2 (c)
   ! passing expressions
   a(1,1) = 41
   a(5,8) = 42
   call f1(a+1)
   call f2(a+1)
   a(1,1) = 42
   a(5,8) = 43
   call f1 ((a + b) / 2)
   call f2 ((a + b) / 2)
   ! Passing whole arrays as sections
   call f1 (a(:,:))
   call f1 (b(:,:))
   call f1 (c(:,:))
   call f2 (a(:,:))
   call f2 (b(:,:))
   call f2 (c(:,:))
   ! Passing sections
   e(:,:) = 0
   e(2, 3) = 42
   e(6, 10) = 43
   n = 3
   call f1 (e(2:6, n:10))
   call f2 (e(2:6, n:10))
   ! Vector subscripts
   ! v1= index plus one, v2(3, ::2) = reverse of index
   e(:,:) = 0
   e(2, 3) = 42
   e(6, 10) = 43
   call f1 (e(v1, n:10))
   call f2 (e(v1, n:10))
   ! Double vector subscript
   e(:,:) = 0
   e(6, 3) = 42
   e(2, 10) = 43
   !These are not resolved properly
   call f1 (e(v1(v2(3, ::2)), n:10))
   call f2 (e(v1(v2(3, ::2)), n:10))
   ! non-contiguous sections
   e(:,:) = 0
   e(1, 1) = 42
   e(13, 22) = 43
   n = 3
   call f1 (e(1:15:3, 1:24:3))
   call f2 (e(::3, ::n))
   ! non-contiguous sections with bounds
   e(:,:) = 0
   e(3, 4) = 42
   e(11, 18) = 43
   n = 19
   call f1 (e(3:11:2, 4:n:2))
   call f2 (e(3:11:2, 4:n:2))

   ! Passing a dummy variable
   call f1 (parm)
   call f2 (parm)
end subroutine
! Assumed shape parameter
subroutine f2 (a)
   integer, dimension (1:, 1:) :: a

   if (a(1, 1) .ne. 42) call abort

   if (a(5, 8) .ne. 43) call abort
end subroutine
end program

