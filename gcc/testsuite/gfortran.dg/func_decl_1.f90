! { dg-do compile }
! we didn't correctly reject function declarations without argument lists
! note that there are no end statements for syntactically wrong function
! declarations
  interface
     function f1     ! { dg-error "Expected formal argument list" }
     function f3()
     end function f3
     function f4 result (x) ! { dg-error "Expected formal argument list" }
     function f5() result (x)
     end function f5
   end interface
  f1 = 1.
end

FUNCTION f1          ! { dg-error "Expected formal argument list" }

function f2()
  f2 = 1.
end function f2

function f3 result (x) ! { dg-error "Expected formal argument list" }

function f4 () result (x)
  x = 4.
end function f4
