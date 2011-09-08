! { dg-do compile }
!
! PR fortran/50163
!
! Contributed by Philip Mason
!
character(len=2) :: xx ='aa'
integer :: iloc=index(xx,'bb') ! { dg-error "has not been declared or is a variable" }
end
