! { dg-do run }
! { dg-options "-ffloat-store" }
! PR29053 Stream IO test 9.
! Contributed by Jerry DeLisle <jvdelisle@gcc.gnu.org>.
! Test case derived from that given in PR by Steve Kargl.
program pr29053
   implicit none
   real dt, t, u, a(10), b(10)
   integer i, place
   dt = 1.e-6
   a = real( (/ (i, i=1, 10) /) )
   b = a
   open(unit=11, file='a.dat', access='stream')
   open(unit=12, file='b.dat', access='stream')
   do i = 1, 10
      t = i * dt
      write(11) t
      write(12) a
   end do
   rewind(11)
   rewind(12)
   do i = 1, 10
      t = i * dt
      read(12) a
      if (any(a.ne.b)) STOP 1
      read(11) u
      if (u.ne.t) STOP 2
   end do
   close(11, status="delete")
   close(12, status="delete")
end program pr29053

