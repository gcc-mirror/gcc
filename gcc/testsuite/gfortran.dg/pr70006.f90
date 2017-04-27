! { dg-do compile }
program test
   print 1, 'string 1' ! { dg-error "FORMAT label 1" " " { target *-*-* } . }
   print 1, 'string 2' ! { dg-error "FORMAT label 1" " " { target *-*-* } . }
!1 format(a)
   goto 2 ! { dg-error "Label 2 referenced" " " { target *-*-* } . }
   goto 2 ! { dg-error "Label 2 referenced" " " { target *-*-* } . }
!2 continue
end program
