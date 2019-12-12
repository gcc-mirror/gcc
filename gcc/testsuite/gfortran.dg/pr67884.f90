! { dg-do compile }
! PR fortran/67884
! Original code contribute by Gerhard Steinmetz 
program p
   integer i
   print *, [(f(i), i=1,3)]
   print *, [(g(i), i=1,3)]
   contains
   function f(n)              ! { dg-error "has a deferred type parameter" }
      integer :: n
      character(:) :: f
      character(3) :: c = 'abc'
      f = c(n:n)
   end
   function g(n) result(z)    ! { dg-error "has a deferred type parameter" }
      integer :: n
      character(:) :: z
      character(3) :: c = 'abc'
      z = c(n:n)
   end
end program p
