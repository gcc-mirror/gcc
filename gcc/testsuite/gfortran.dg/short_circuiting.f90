! { dg-do compile }
! { dg-additional-options "-Wextra" }
!
! PR 85599: warn about short-circuiting of logical expressions for non-pure functions
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module a

   interface impl_pure_a
      module procedure impl_pure_a1
   end interface

contains

    logical function impl_pure_a1()
      impl_pure_a1 = .true.
   end function

end module


program short_circuit

   use a

   logical :: flag
   flag = .false.
   flag = check() .and. flag
   flag = flag .and. check()        ! { dg-warning "might not be evaluated" }
   flag = flag .and. pure_check()
   flag = flag .and. impl_pure_1()
   flag = flag .and. impl_pure_2()
   flag = flag .and. impl_pure_a1()
   flag = flag .and. impl_pure_a()

contains

   logical function check()
      integer, save :: i = 1
      print *, "check", i
      i = i + 1
      check = .true.
   end function

   logical pure function pure_check()
      pure_check = .true.
   end function

   logical function impl_pure_1()
      impl_pure_1 = .true.
   end function

   logical function impl_pure_2()
      impl_pure_2 = impl_pure_1()
   end function


end
