! { dg-do compile }
! PR 18197: Check that dummy functions with RESULT variable and dimension works.
module innerfun
contains
  function f(n,x) result(y)
    integer, intent(in) :: n
    real, dimension(:), intent(in) :: x
    real, dimension(n) :: y
    y = 1
  end function f
end module innerfun

module outerfun
contains
   subroutine foo(n,funname)
     integer, intent(in) :: n
     real, dimension(n) :: y
     real, dimension(2) :: x
     interface
       function funname(n,x) result(y)
         integer, intent(in) :: n
         real, dimension(:), intent(in) :: x
         real, dimension(n)  :: y
       end function funname
     end interface

     y = funname(n, (/ 0.2, 0.3 /) )

   end subroutine foo
end module outerfun

program test
   use outerfun
   use innerfun
   call foo(3,f)
end program test

! { dg-final { cleanup-modules "innerfun outerfun" } }
