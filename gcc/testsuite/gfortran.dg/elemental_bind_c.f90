! {dg-do compile }
!
! PR fortran/33412
!
elemental subroutine a() bind(c) ! { dg-error "BIND.C. attribute conflicts with ELEMENTAL" }
end subroutine a ! { dg-error "Expecting END PROGRAM" }

elemental function b() bind(c) ! { dg-error "BIND.C. attribute conflicts with ELEMENTAL" }
end function b ! { dg-error "Expecting END PROGRAM" }
end
