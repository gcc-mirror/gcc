! { dg-do compile }
! PR66461  ICE on missing end program in fixed source
      program p
         integer x(2)
         x = -1
         if ( x(1) < 0 .or.
     &        x(2) < 0 ) print *, x
! { dg-error "Unexpected end of file" "" { target *-*-* } 0 }
