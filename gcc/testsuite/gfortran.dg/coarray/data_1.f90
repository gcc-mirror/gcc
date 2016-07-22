! { dg-do compile }
!
! PR fortran/71068
!
! Contributed by Gerhard Steinmetz
!
program p
   integer :: a(2)[*]
   data a(1)[1] /1/  ! { dg-error "cannot have a coindex" }
   data a(2)[1] /2/  ! { dg-error "cannot have a coindex" }
end
