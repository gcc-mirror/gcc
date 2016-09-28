! { dg-do compile}
program test
   print 1, 'string 1' ! { dg-error "FORMAT label 1" " " { target *-*-* } 3 }
   print 1, 'string 2' ! { dg-error "FORMAT label 1" " " { target *-*-* } 4 }
!1 format(a)
   goto 2 ! { dg-error "Label 2 referenced" " " { target *-*-* } 6 }
   goto 2 ! { dg-error "Label 2 referenced" " " { target *-*-* } 7 }
!2 continue
end program
