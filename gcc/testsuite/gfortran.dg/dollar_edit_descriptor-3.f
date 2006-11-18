! { dg-do run }
! { dg-options "-std=gnu" }
! Test for dollar descriptor in the middle of a format
300   format(1000(a,$)) ! { dg-warning "should be the last specifier" }
      write(*,300) "gee", "gee"
      write(*,"(1000(a,$))") "foo", "bar" ! { dg-warning "should be the last specifier" }
      end
! { dg-output "^geegeefoobar$" }
