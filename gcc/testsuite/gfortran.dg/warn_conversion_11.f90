! { dg-do compile }
! { dg-options "-Wconversion" }
! PR 86119 - this used to warn.
program proglen

implicit none

   class(*), allocatable :: s
   integer :: l2

   allocate(s, source = '123  ')

   select type(s)
   type is (character(len=*))
      l2 = len(s)
   end select

end program proglen
