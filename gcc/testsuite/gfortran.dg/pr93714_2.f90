! { dg-do compile }
! PR 93714
! Original test case from G. Steinmetz

program test
   character((9.)) :: a
   character(:), pointer :: b => a
end program

! { dg-error "Scalar INTEGER expression expected" " " { target *-*-* } 6 }
! { dg-error "Different types in pointer assignment" " " { target *-*-* } 7 }
