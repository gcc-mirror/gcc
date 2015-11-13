! { dg-do compile }
! PR fortran/68319
!
subroutine foo

   interface

      real function bar(i)
         f(i) = 2 * i         ! { dg-error "cannot appear within" }
      end function bar

      real function bah(j)
         entry boo(j)         ! { dg-error "cannot appear within" }
      end function bah

      real function fu(j)
         data i /1/           ! { dg-error "cannot appear within" }
      end function fu

      real function fee(j)
10       format('(A)')        ! { dg-error "cannot appear within" }
      end function fee

   end interface

end subroutine foo
