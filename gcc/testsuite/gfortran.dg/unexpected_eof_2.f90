! { dg-do compile }
! PR71686
program p
   character(8) :: z
   z = 'abc&  ! { dg-error "Unterminated character constant" }
!end
! { dg-error "Unexpected end of file" "" { target *-*-* } 0 }
