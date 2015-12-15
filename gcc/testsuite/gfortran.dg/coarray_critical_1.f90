! { dg-do compile }
! { dg-options "-fcoarray=lib" }
!

module m
 contains
   subroutine f()
     critical
     end critical
   end subroutine f
 end module m
end program
