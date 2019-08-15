! { dg-do compile }
! { dg-options -Os }
! PR 78865 - this used to ICE.
program p
   call sub (3)
end
subroutine sub (x)
   integer :: x, i, n
   do i = 1, x
      if ( n /= 0 ) stop
      call sub2
   end do
   print *, x, n
end
subroutine sub2
   call sub (*99) ! { dg-error "Unexpected alternate return specifier" }
   call sub (99.) ! { dg-error "Type mismatch in argument" }
99 stop
end
