! { dg-do compile }
! PR32489 Endless loop when compiling.
! Derived from fft257.f90, Public domain 2004 James Van Buskirk.
! Note: The problem solved here was not an infinite loop issue. Middle-end
! could not handle the array constructor unfolded by the front end.
! WARNING: Potential resource hog.
! Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program test
   implicit none
   integer, parameter :: dp = selected_real_kind(15,300)
   integer, parameter :: N = 257
   complex(dp) h1(0:N-1)
   complex(dp) h2(0:N-1)
   complex(dp) hh(0:N-1)
   complex(dp), parameter :: ri(2) = (/(1,0),(0,1)/)
   integer i, j, k, L
   real(dp) pi

   pi = 4*atan(1.0_dp)
   do i = 0, N-1
     do j = 1, 2
       h2 = 0
       h2(i) = ri(j)
       h1 = (/(sum((/(exp(-2*pi*(0,1)*mod(k*L,N)/N)*h2(L),L=0,N-1)/)),k=0,N-1)/)
     end do
   end do
end program test 
