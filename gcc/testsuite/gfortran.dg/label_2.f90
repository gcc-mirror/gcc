! { dg-do compile }
! PR fortran/24640.  We needed to check that whitespace follows
! a statement label in free form.
!
program pr24640

10: a=10   ! { dg-error "character in statement" }

end program

