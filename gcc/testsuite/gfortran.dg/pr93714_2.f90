! { dg-do compile }
! PR 93714
! Original test case from G. Steinmetz

program test
   character((9.)) :: a
   character(:), pointer :: b => a
end program

! { dg-error "must be of INTEGER type" " " { target *-*-* } 6 }
! { dg-error "does not have the TARGET attribute" " " { target *-*-* } 7 }
