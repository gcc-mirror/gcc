! { dg-do run }
!
! Test results from the F2018 intrinsic REDUCE
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!

module operations
   type :: s
      integer, allocatable :: i
      integer :: j
   end type s

contains

   pure function add(i,j) result(sum_ij)
      integer, intent(in) :: i, j
      integer             :: sum_ij
      sum_ij = i + j
   end function add
!
   pure function mult(i,j) result(prod_ij)
      integer, intent(in) :: i, j
      integer             :: prod_ij
      prod_ij = i * j
   end function mult

   pure function mult_by_val(i,j) result(prod_ij)
      integer, intent(in), value :: i, j
      integer             :: prod_ij
      prod_ij = i * j
   end function mult_by_val

   pure function non_com(i,j) result(nc_ij)
      integer, intent(in) :: i, j
      integer             :: nc_ij
      if (i > j) then
        nc_ij = i - j
      else
        nc_ij = i + j
      endif
   end function non_com

   pure function c_op (i, j) result (ij)
      character(8), intent(in) :: i, j
      character(8) :: ij
      integer :: n
      ij = i
      do n = 1, 8
         if (i(n:n) .ne. j(n:n)) ij(n:n) = '!'
      end do
   end function c_op

   pure function t_op (i, j) result (ij)
      type(s), intent(in) :: i, j
      type(s) :: ij
      ij%i = non_com (i%i, j%i)
      ij%j = non_com (j%j, i%j)
   end function t_op

   pure function t_add (i, j) result (ij)
      type(s), intent(in) :: i, j
      type(s) :: ij
      ij%i = i%i + j%i
      ij%j = j%j + i%j
   end function t_add
end module operations

program test_reduce
   use operations
   implicit none
   integer :: i
   integer, parameter :: n = 3
   integer, parameter :: vec(n) = [2, 5, 10]
   integer, parameter :: mat(n,2) = reshape([vec,2*vec],shape=[size(vec),2])
   integer :: res0
   integer, dimension(:), allocatable :: res1
   integer, dimension(:,:), allocatable :: res2
   logical, parameter :: t = .true., f = .false.
   LOGICAL, PARAMETER :: keep(n) = [t,f,t]
   logical, parameter :: keepM(n,2) = reshape([keep,keep],shape=[n,2])
   logical, parameter :: all_false(n,2) = reshape ([(f, i = 1,2*n)],[n,2])
   character(*), parameter :: carray (4) = ['abctefgh', 'atcdefgh', &
                                            'abcdefth', 'abcdtfgh']
   character(:), allocatable :: cres0, cres1(:)
   type(s), allocatable :: tres1(:)
   type(s), allocatable :: tres2(:,:)
   type(s) :: tres2_na(2, 4)
   type(s), allocatable :: tarray(:,:,:)
   type(s), allocatable :: tvec(:)
   type(s), allocatable :: tres0
   integer, allocatable :: ires(:)

! Simple cases with and without DIM
   res0 = reduce (vec, add, dim=1)
   if (res0 /= 17) stop 1
   res0 = reduce (vec, mult, 1)
   if (res0 /= 100) stop 2
   res1 = reduce (mat, add, 1)
   if (any (res1 /= [17, 34])) stop 3
   res1 = reduce (mat, mult, 1)
   if (any (res1 /= [100, 800])) stop 4
   res1 = reduce (mat, add, 2)
   if (any (res1 /= [6, 15, 30])) stop 5
   res1 = reduce (mat, mult, 2)
   if (any (res1 /= [8, 50, 200])) stop 6
   res0 = reduce (mat, add)
   if (res0 /= 51) stop 7
   res0 = reduce (mat, mult)
   if (res0 /= 80000) stop 8
! Repeat previous test with arguments passed by value to operation
   res0 = reduce (mat, mult_by_val)
   if (res0 /= 80000) stop 9

! Using MASK and IDENTITY
   res0 = reduce (vec,add, mask=keep, identity = 1)
   if (res0 /= 12) stop 10
   res0 = reduce (vec,mult, mask=keep, identity = 1)
   if (res0 /= 20) stop 11
   res0 = reduce (mat, add, mask=keepM, identity = 1)
   if (res0 /= 36) stop 12
   res0 = reduce (mat, mult, mask=keepM, identity = 1)
   if (res0 /= 1600) stop 13
   res0 = reduce (mat, mult, mask=all_false, identity = -1)
   if (res0 /= -1) stop 14

! 3-D ARRAYs with and without DIM and MASK
   res0 = reduce (reshape ([(i, i=1,8)], [2,2,2]),mult)
   if (res0 /= 40320) stop 15
   res2 = reduce (reshape ([(i, i=1,8)], [2,2,2]),mult,dim=2)
   if (any (res2 /= reshape ([3,8,35,48], [2,2]))) stop 16
   res2 = reduce (reshape ([(i, i=1,8)], [2,2,2]),mult,dim=2, &
                  mask=reshape ([t,f,t,f,t,f,t,f],[2,2,2]), identity=-1)
   if (any (res2 /= reshape ([3,-1,35,-1], [2,2]))) stop 17
   res2 = reduce (reshape([(i, i=1,16)], [2,4,2]), add, dim = 3, &
                  mask=reshape([f,t,t,f,t,t,t,t,t,t,t,t,t,t,t,t],[2,4,2]), &
                  identity=-1)
   if (any (res2 /= reshape ([9,12,14,12,18,20,22,24], [2,4]))) stop 18
   res1 = reduce (reshape([(i, i=1,16)], [4,4]),add, dim = 2, &
                  mask=reshape([f,t,t,f,t,t,t,t,t,t,t,t,t,t,t,t],[4,4]), &
                  identity=-1)
   if (any (res1 /= [27,32,36,36])) stop 19

! Verify that the library function treats non-comutative OPERATION in the
! correct order. If this were incorrect,the result would be [9,8,8,12,8,8,8,8].
   res2 = reduce (reshape([(i, i=1,16)], [2,4,2]), non_com, dim = 3, &
                  mask=reshape([f,t,t,f,t,t,t,t,t,t,t,t,t,t,t,t],[2,4,2]), &
                  identity=-1)
   if (any (res2 /= reshape([9,12,14,12,18,20,22,24],shape(res2)))) stop 20

! Character ARRAY and OPERATION
   cres0 = reduce (carray, c_op); if (cres0 /= 'a!c!!f!h') stop 21
   cres1 = reduce (reshape (carray, [2,2]), c_op, dim = 1)
   if (any (cres1 /= ['a!c!efgh','abcd!f!h'])) stop 22

! Derived type ARRAY and OPERATION - was checked for memory leaks of the
! allocatable component.
! tarray = reshape([(s(i, i), i = 1, 16)], [2,4,2]) leaks memory!
   allocate (tvec(16))
   do i = 1, 16
     tvec(i)%i = i
     tvec(i)%j = i
   enddo
   tarray = reshape(tvec, [2,4,2])

   tres2 = reduce (tarray, t_op, dim = 3, &
                   mask=reshape([t,t,t,f,t,t,t,t,t,f,t,t,t,t,t,t],[2,4,2]), &
                   identity = s(NULL(),1))
   ires = [10,2,14,12,18,20,22,24]
   tres1 = reshape (tres2, [size (tres2, 1)* size (tres2, 2)])
   do i = 1, size (tres2, 1)* size (tres2, 2)
      if (tres1(i)%i /= ires(i)) stop 23
   end do
   if (any (tres2%j /= reshape([8,2,8,12,8,8,8,8],shape(tres2)))) stop 24

! Check that the non-allocatable result with an allocatable component does not
! leak memory from the allocatable component
   tres2_na = reduce (tarray, t_op, dim = 3, &
                      mask=reshape([t,t,t,f,t,t,t,t,t,f,t,t,t,t,t,t],[2,4,2]), &
                      identity = s(NULL(),1))
   tres1 = reshape (tres2_na, [size (tres2_na, 1)* size (tres2, 2)])
   do i = 1, size (tres2_na, 1)* size (tres2_na, 2)
      if (tres1(i)%i /= ires(i)) stop 25
   end do
   if (any (tres2_na%j /= reshape([8,2,8,12,8,8,8,8],shape(tres2_na)))) stop 26


   tres0 = reduce (tarray, t_add)
   if (tres0%i /= 136) stop 27
   if (tres0%j /= 136) stop 28

! Test array being a component of an array of derived types
   i = reduce (tarray%j, add, &
               mask=reshape([t,t,t,f,t,t,t,t,t,f,t,t,t,t,f,t],[2,4,2]), &
               identity = 0)
   if (i /= 107) stop 29


! Deallocate the allocatable components and then the allocatable variables
   tres2_na = reshape ([(s(NULL (), 0), i = 1, size (tres2_na))], shape (tres2_na))
   deallocate (res1, res2, cres0, cres1, tarray, ires, tres0, tres1, tres2, tvec)
end
