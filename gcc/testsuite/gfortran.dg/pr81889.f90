! { dg-do compile }
! { dg-options "-O3 -Wall" }

module m

   type t
      integer, dimension(:), pointer :: list
   end type

contains

   subroutine s(n, p, Y)
      integer, intent(in) :: n
      type(t) :: p
      real, dimension(:) :: Y

      real, dimension(1:16) :: xx

      if (n > 3) then
         xx(1:n) = 0.
         print *, xx(1:n)
      else
         xx(1:n) = Y(p%list(1:n)) ! { dg-bogus "uninitialized" }
         print *, sum(xx(1:n))
      end if

   end subroutine

end module
