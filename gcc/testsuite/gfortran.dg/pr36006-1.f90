! { dg-do compile }
!
subroutine test4
  integer, parameter :: wp = 4
  complex(wp), parameter :: i = (0._wp, 1._wp)
  complex(wp) :: c(12)
  integer :: m, N

  N = 12
  c = (/(exp(i*m),m=1,N)/)
  print *, c(1)
end

subroutine test8
  integer, parameter :: wp = 8
  complex(wp), parameter :: i = (0._wp, 1._wp)
  complex(wp) :: c(12)
  integer :: m, N

  N = 12
  c = (/(exp(i*m),m=1,N)/)
  print *, c(1)
end
