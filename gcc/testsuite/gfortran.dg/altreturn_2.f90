! { dg-do compile }
! { dg-options "-std=gnu" }

       program altreturn_2
         call foo()  ! { dg-error "Missing alternate return" }
       contains
         subroutine foo(*)
           return
         end subroutine
       end program
