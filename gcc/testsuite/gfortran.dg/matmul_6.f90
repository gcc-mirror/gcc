! { dg-do run }
! PR 34566 - logical matmul used to give the wrong result.
! We check this by running through every permutation in
! multiplying two 3*3 matrices, and all permutations of multiplying
! a 3-vector and a 3*3 matrices  and checking against equivalence
! with integer matrix multiply.
program main
  implicit none
  integer, parameter :: ki=4
  integer, parameter :: dimen=3
  integer :: i, j, k
  real, dimension(dimen,dimen) :: r1, r2
  integer, dimension(dimen,dimen) :: m1, m2
  logical(kind=ki), dimension(dimen,dimen) :: l1, l2
  logical(kind=ki), dimension(dimen*dimen) :: laux
  logical(kind=ki), dimension(dimen) :: lv
  integer, dimension(dimen) :: iv

  do i=0,2**(dimen*dimen)-1
     forall (k=1:dimen*dimen)
        laux(k) = btest(i, k-1)
     end forall
     l1 = reshape(laux,shape(l1))
     m1 = ltoi(l1)

     ! Check matrix*matrix multiply
     do j=0,2**(dimen*dimen)-1
        forall (k=1:dimen*dimen)
           laux(k) = btest(i, k-1)
        end forall
        l2 = reshape(laux,shape(l2))
        m2 = ltoi(l2)
        if (any(matmul(l1,l2) .neqv. (matmul(m1,m2) /= 0))) then
          call abort
        end if
     end do

     ! Check vector*matrix and matrix*vector multiply.
     do j=0,2**dimen-1
        forall (k=1:dimen)
           lv(k) = btest(j, k-1)
        end forall
        iv = ltoi(lv)
        if (any(matmul(lv,l1) .neqv. (matmul(iv,m1) /=0))) then
          call abort
        end if
        if (any(matmul(l1,lv) .neqv. (matmul(m1,iv) /= 0))) then
          call abort
        end if
     end do
  end do

contains
  elemental function ltoi(v)
    implicit none
    integer :: ltoi
    real :: rtoi
    logical(kind=4), intent(in) :: v
    if (v) then
       ltoi = 1
    else
       ltoi = 0
    end if
  end function ltoi

end program main
