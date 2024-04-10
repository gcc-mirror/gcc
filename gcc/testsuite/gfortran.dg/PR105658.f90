! { dg-do compile }
! { dg-options "-Warray-temporaries" }
! Test fix for incorrectly passing array component to unlimited polymorphic procedure

module test_PR105658_mod
   implicit none
   type :: foo
     integer :: member1
     integer :: member2
   end type foo
contains
   subroutine print_poly(array)
     class(*), dimension(:), intent(in) :: array
     select type(array)
     type is (integer)
       print*, array
     type is (character(*))
       print *, array
     end select
   end subroutine print_poly

   subroutine do_print(thing)
     type(foo), dimension(3), intent(in) :: thing
     type(foo), parameter :: y(3) = [foo(1,2),foo(3,4),foo(5,6)]
     integer :: i, j, uu(5,6)

     call print_poly(thing%member1)   ! { dg-warning "array temporary" }
     call print_poly(y%member2)       ! { dg-warning "array temporary" }
     call print_poly(y(1::2)%member2) ! { dg-warning "array temporary" }

     ! The following array sections work without temporaries
     uu = reshape([(((10*i+j),i=1,5),j=1,6)],[5,6])
     print *, uu(2,2::2)
     call print_poly (uu(2,2::2))     ! no temp needed!
     print *, uu(1::2,6)
     call print_poly (uu(1::2,6))     ! no temp needed!
   end subroutine do_print

   subroutine do_print2(thing2)
     class(foo), dimension(:), intent(in) :: thing2
     call print_poly (thing2% member2) ! { dg-warning "array temporary" }
   end subroutine do_print2

   subroutine do_print3 ()
     character(3) :: c(3) = ["abc","def","ghi"]
     call print_poly (c(1::2))      ! no temp needed!
     call print_poly (c(1::2)(2:3)) ! { dg-warning "array temporary" }
   end subroutine do_print3

end module test_PR105658_mod
