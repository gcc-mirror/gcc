! { dg-do compile }
! { dg-options "-pedantic -mdalign" { target sh*-*-* } }
!
! PR fortran/50273
!
subroutine test()
   character :: a
   integer   :: b
   character :: c
   common /global_var/ a, b, c ! { dg-warning "Padding of 3 bytes required before 'b' in COMMON" }
   print *, a, b, c
end subroutine test
