! Snippet to test various allocate statements

program test_allocate
   implicit none
   type t
      integer i
      real r
   end type
   type pt
      integer, pointer :: p
   end type
   integer, allocatable, dimension(:, :) :: a
   type (t), pointer, dimension(:) :: b
   type (pt), pointer :: c
   integer, pointer:: p
   integer n

   n = 10
   allocate (a(1:10, 4))
   allocate (a(5:n, n:14))
   allocate (a(6, 8))
   allocate (b(n))
   allocate (c)
   allocate (c%p)
   allocate (p)
end program
