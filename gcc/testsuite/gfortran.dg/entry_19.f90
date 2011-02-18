! { dg-do compile }
! { dg-options "-std=f2008" }
! 
!
! Entry is obsolete in Fortran 2008
!
subroutine foo()
entry bar() ! { dg-warning "Fortran 2008 obsolescent feature: ENTRY" }
end 
