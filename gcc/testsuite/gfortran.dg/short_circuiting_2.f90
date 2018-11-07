! { dg-do run }
! { dg-options "-O0" }
!
! PR 57160: short-circuit IF only with -ffrontend-optimize
!
! this checks that short-circuiting is not done with -O0
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

program short_circuit

   integer, save :: i = 0
   logical :: flag

   flag = .false.
   flag = check() .and. flag
   flag = flag .and. check()

   if (i /= 2) stop 1

contains

   logical function check()
      i = i + 1
      check = .true.
   end function

end
