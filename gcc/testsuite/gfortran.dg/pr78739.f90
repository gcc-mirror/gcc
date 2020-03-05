! { dg-do compile }
! { dg-options "-w" }
! PR fortran/78739
! Code contributed Gerhard Steinmetz
function f(n)
   f() = n    ! { dg-error "conflicts with function name" }
end

function g()
   g(x) = x   ! { dg-error "conflicts with function name" }
end

function a()  ! This should cause an error, but cannot be easily detected!
   a() = x
end
