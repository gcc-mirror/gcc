! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Check the fix for the testcase in comment 4, where the hidden string length
! component of the array pointer component was not set.
!
! Contributed by Sebastien Bardeau <bardeau@iram.fr>
!
program test2
  implicit none
  character(len=10), allocatable, target :: s(:)
  character(len=:),  pointer             :: sptr(:)
  type :: pointer_typec0_t
    character(len=:), pointer :: data0
    character(len=:), pointer :: data1(:)
  end type pointer_typec0_t
  type(pointer_typec0_t) :: co
  !
  allocate(s(3))
  s(1) = '1234567890'
  s(2) = 'qwertyuio '
  s(3) = 'asdfghjk  '
  !
  sptr => s
  co%data0 => s(1)
  co%data1 => s
  !
  if (any (sptr .ne. s)) stop 1
  if (co%data0 .ne. s(1)) stop 2
  if (any (co%data1 .ne. s)) stop 3 ! Hidden string length was not set
end program test2
! { dg-final { scan-tree-dump-times "co._data1_length = 10;" 1 "original" } }