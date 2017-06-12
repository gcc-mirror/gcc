! { dg-do compile }
program test
   print 1, 'string 1' ! { dg-error "FORMAT label 1" " " }
   print 1, 'string 2' ! { dg-error "FORMAT label 1" " " }
!1 format(a)
   goto 2 ! { dg-error "Label 2 referenced" " " }
   goto 2 ! { dg-error "Label 2 referenced" " " }
!2 continue
end program
