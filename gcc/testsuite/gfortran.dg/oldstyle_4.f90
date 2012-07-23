! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/52101
!
! Contributed by John Harper
!
program foo
   character*10 s    ! { dg-warning "Obsolescent feature: Old-style character length" }
   character    t*10 ! Still okay
   s = 'foo'
   t = 'bar'
end program foo
