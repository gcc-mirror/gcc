! { dg-do compile }
!
! See PR fortran/31610
!
implicit none
character(len=2) :: a
character(len=3) :: b
print *, merge(a,a,.true.)
print *, merge(a,'aa',.true.)
print *, merge('aa',a,.true.)
print *, merge('aa','bb',.true.)
print *, merge(a,   b,    .true.)  ! { dg-error "Unequal character lengths" }
print *, merge(a,   'bbb',.true.)  ! { dg-error "Unequal character lengths" }
print *, merge('aa',b,    .true.)  ! { dg-error "Unequal character lengths" }
print *, merge('aa','bbb',.true.)  ! { dg-error "Unequal character lengths" }
end
