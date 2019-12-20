! { dg-do compile }
! { dg-additional-options "-fcoarray=single" }
! 
subroutine check_coindexed()
implicit none
type t
  integer :: i
end type t
type t2
  integer, allocatable :: i[:]
  type(t), allocatable :: x[:]
end type t2
type(t), allocatable :: A(:)[:], B(:)[:]
type(t) :: D(1)[*], E[*]
type(t2) :: C
save :: D, E

! Coarrays are fine if they are local/not coindexed:

!$acc enter data copyin(D(1)%i)
!$acc enter data copyin(A(1))
!$acc enter data copyin(B(1)%i)
!$acc enter data copyin(C%i)
!$acc enter data copyin(C%x%i) 
!$acc enter data copyin(C%i)
!$acc enter data copyin(C%x%i) 

! Does not like the '[' after the identifier:
!$acc enter data copyin(E[2]) ! { dg-error "Syntax error in OpenMP variable list" }

!$acc enter data copyin(D(1)[2]%i) ! { dg-error "List item shall not be coindexed" }
!$acc enter data copyin(A(1)[4])   ! { dg-error "List item shall not be coindexed" }
!$acc enter data copyin(B(1)[4]%i) ! { dg-error "List item shall not be coindexed" }
!$acc enter data copyin(C%i[2])    ! { dg-error "List item shall not be coindexed" }
!$acc enter data copyin(C%x[4]%i)  ! { dg-error "List item shall not be coindexed" }

end
