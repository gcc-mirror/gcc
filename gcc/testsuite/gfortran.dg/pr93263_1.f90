! { dg-do compile }
! { dg-options "-fno-automatic -fdump-tree-original" }
!
! Test contributed by Mark Eggleston  <mark.eggleston@codethink.com>

program main
  implicit none
  call check(2)
end 

recursive subroutine check(n)
  implicit none
  integer n, a
  a = 10
  print*,"n=",n
  if (n==1) then
    a=a-1
    print*,"assigning a=",a
  else
    a=a-2
    print*,"assigning a=",a
    call check(n-1)
  endif
  print*,"a=",a
end 

! { dg-final { scan-tree-dump-not "static integer\\(kind=4\\) a" "original" } }
! { dg-final { scan-tree-dump "integer\\(kind=4\\) a" "original" } }

