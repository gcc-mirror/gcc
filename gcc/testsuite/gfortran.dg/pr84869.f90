! { dg-do compile }
!
! Test the fix for PR84869, where line 19 segfaulted.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type t
     integer :: i
   end type
   call s
contains
   function f()
      class(t), allocatable :: f(:)
      f = [(t(i), i = 1, 10)]
   end
   subroutine s
      class(*), allocatable :: z(:)
      allocate (z, source = f ()) ! Segfault in gfc_class_len_get.
      select type (z)
        type is (t)
          if (any (z%i /= [(i, i = 1,10)])) stop 1
      end select
   end
end
