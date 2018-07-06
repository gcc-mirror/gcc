module uops
   implicit none
   interface operator (.foo.)
      module procedure myfoo
   end interface

   interface operator (*)
      module procedure boolmul
   end interface

   interface assignment (=)
      module procedure int2bool
   end interface

contains
function myfoo (lhs, rhs)
   implicit none
   integer myfoo
   integer, intent(in) :: lhs, rhs

   myfoo = lhs + rhs
end function

! This is deliberately different from integer multiplication
function boolmul (lhs, rhs)
   implicit none
   logical boolmul
   logical, intent(IN) :: lhs, rhs

   boolmul = lhs .and. .not. rhs
end function

subroutine int2bool (lhs, rhs)
   implicit none
   logical, intent(out) :: lhs
   integer, intent(in) :: rhs

   lhs = rhs .ne. 0
end subroutine
end module

program me
   use uops
   implicit none
   integer i, j
   logical b, c

   b = .true.
   c = .true.
   if (b * c) STOP 1
   c = .false.
   if (.not. (b * c)) STOP 2
   if (c * b) STOP 3
   b = .false.
   if (b * c) STOP 4

   i = 0
   b = i
   if (b) STOP 5
   i = 2
   b = i
   if (.not. b) STOP 6

   j = 3
   if ((i .foo. j) .ne. 5) STOP 7
end program

