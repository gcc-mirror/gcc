! { dg-do compile }
! { dg-options "-std=f95" }
! PR fortran/78278
program p
   character, pointer :: x => null()
   data x /null()/         ! { dg-error "GNU Extension: re-initialization" }
   print *, associated(x)
end

subroutine foo
   real :: x = 42
   data x /0/              ! { dg-error "GNU Extension: re-initialization" }
   print *, x
end subroutine foo
