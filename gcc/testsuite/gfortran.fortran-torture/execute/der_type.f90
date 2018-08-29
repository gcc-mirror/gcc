! Program to test derived types
program der_type
   implicit none
   type t1
      integer, dimension (4, 5) :: a
      integer :: s
   end type
   
   type my_type
      character(20) :: c
      type (t1), dimension (4, 3) :: ca
      type (t1) :: r
   end type

   type init_type
      integer :: i = 13
      integer :: j = 14
   end type

   type (my_type) :: var
   type (init_type) :: def_init
   type (init_type) :: is_init = init_type (10, 11)
   integer i;

   if ((def_init%i .ne. 13) .or. (def_init%j .ne. 14)) STOP 1
   if ((is_init%i .ne. 10) .or. (is_init%j .ne. 11)) STOP 2
   ! Passing a component as a parameter tests getting the addr of a component
   call test_call(def_init%i)
   var%c = "Hello World"
   if (var%c .ne. "Hello World") STOP 3
   var%r%a(:, :) = 0
   var%ca(:, :)%s = 0
   var%r%a(1, 1) = 42
   var%r%a(4, 5) = 43
   var%ca(:, :)%s = var%r%a(:, 1:5:2)
   if (var%ca(1, 1)%s .ne. 42) STOP 4
   if (var%ca(4, 3)%s .ne. 43) STOP 5
contains
   subroutine test_call (p)
      integer  p

      if (p .ne. 13) STOP 6
   end subroutine
end program

