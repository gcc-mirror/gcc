! { dg-do compile }
!
! Original by G. Steinmetz

function f(x)
  integer, intent(in) :: x
  integer :: f
  f = x
end

program p
  interface
    function f(x) ! { dg-error "POINTER attribute mismatch in function result" }
      integer, intent(in) :: x
      integer, pointer :: f
    end
  end interface
  integer :: a(2) = [1, 2]
  if ( a(2) .ne. f(a(2)) ) stop 1
end

