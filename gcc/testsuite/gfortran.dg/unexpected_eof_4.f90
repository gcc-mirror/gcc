! { dg-do compile }
! { dg-options "-Wampersand" }
! PR77972
program p
   character(8) :: z
   z = 'abc&
! { dg-error "Unterminated character constant" "" { target *-*-* } 0  }
! { dg-error "Unexpected end of file" "" { target *-*-* } 0 }
