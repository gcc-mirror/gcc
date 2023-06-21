! { dg-do run }
!
! Contributed by Karl Kaiser  <kaiserkarl31@yahoo.com>
!
program test

   class(*), pointer :: ptr1, ptr2(:)
   integer, target :: i = 42
   integer :: check = 0
! First with associate name and no selector in select types
   associate (c => ptr1)
        select type (c)  ! Segfault - vptr not set
           type is (integer)
              stop 1
           class default
              check = 1
        end select
   end associate
! Now do the same with the array version
   associate (c => ptr2)
        select type (d =>c)  ! Segfault - vptr not set
           type is (integer)
              stop 2
           class default
              check = check + 10
        end select
   end associate

! And now with the associate name and selector
   associate (c => ptr1)
        select type (d => c)  ! Segfault - vptr not set
           type is (integer)
              stop 3
           class default
              check = check + 100
        end select
   end associate
! Now do the same with the array version
!   ptr2 => NULL()            !This did not fix the problem
   associate (c => ptr2)
        select type (d => c)  ! Segfault - vptr not set
           type is (integer)
              stop 4
           class default
              check = check + 1000
        end select
   end associate
   if (check .ne. 1111) stop 5
end program test
