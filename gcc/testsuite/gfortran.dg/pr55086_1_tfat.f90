! { dg-do run }
! { dg-options "-ftest-forall-temp" }
!
  implicit none
  character(len=5), pointer :: a(:), b(:)
  character(len=5), pointer :: c, d
  allocate (a(2), b(2), c, d)
  a = [ "abcde", "ABCDE" ]
  call aloct_pointer_copy_4 (b, a)
  !print *, b(1)
  !print *, b(2)
  if (any (a /= b)) stop 'WRONG'

  call aloct_copy_4 (b, a)
  !print *, b(1)
  !print *, b(2)
  if (any (a /= b)) stop 'WRONG'

  d = '12345'
  c = "abcde"
  call test2 (d, c)
  !print *, d
  if (d /= '1cb15') stop 'WRONG'

  call test2p (d, c)
  !print *, d
  if (d /= '1cb15') stop 'WRONG'

contains
 subroutine aloct_pointer_copy_4(o, i)
  character(len=*), pointer :: o(:), i(:)
  integer :: nl1, nu1
  integer :: i1
  nl1 = lbound(i,dim=1)
  nu1 = ubound(i,dim=1)
  forall (i1 = nl1:nu1) o(i1) = i(i1)
 end subroutine aloct_pointer_copy_4
 subroutine aloct_copy_4(o, i)
  character(len=*), pointer :: o(:), i(:)
  integer :: nl1, nu1
  integer :: i1
  nl1 = lbound(i,dim=1)
  nu1 = ubound(i,dim=1)
  forall (i1 = nl1:nu1) o(i1) = i(i1)
 end subroutine aloct_copy_4
 subroutine test2(o, i)
  character(len=*) :: o, i
  integer :: nl1, nu1
  integer :: i1
  nl1 = 2
  nu1 = 4
  forall (i1 = nl1:nu1) o(i1:i1) = i(i1:i1)
  forall (i1 = nl1:nu1) o(i1:i1) = o(nu1+1-i1:nu1+1-i1)
 end subroutine test2
 subroutine test2p(o, i)
  character(len=*), pointer :: o, i
  integer :: nl1, nu1
  integer :: i1
  nl1 = 2
  nu1 = 4
  forall (i1 = nl1:nu1) o(i1:i1) = i(i1:i1)   ! <<<< ICE
  forall (i1 = nl1:nu1) o(i1:i1) = o(nu1+1-i1:nu1+1-i1)
 end subroutine test2p
end
