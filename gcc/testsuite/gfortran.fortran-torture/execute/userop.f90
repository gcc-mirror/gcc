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
   if (b * c) call abort
   c = .false.
   if (.not. (b * c)) call abort
   if (c * b) call abort
   b = .false.
   if (b * c) call abort

   i = 0
   b = i
   if (b) call abort
   i = 2
   b = i
   if (.not. b) call abort

   j = 3
   if ((i .foo. j) .ne. 5) call abort
end program

