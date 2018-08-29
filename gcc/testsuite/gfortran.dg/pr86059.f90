! { dg-do compile }
! PR fortran/86059
program foo
   integer :: i(2) = [ null(), 1 ]           ! { dg-error "cannot appear in an array constructor" }
   integer :: j(2) = [ (null(), n = 1, 2) ]  ! { dg-error "cannot appear in an array constructor" }
   integer k(2)
   k = 42 + [1, null()]                      ! { dg-error "cannot appear in an array constructor" }
end program foo
