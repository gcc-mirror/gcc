! { dg-do compile }
!
! Test case by  G. Steinmetz

program p
   character(len=1, kind=4) :: x(3) = [4_'a', 4_'b', 4_'c']
   character(len=1, kind=1) :: y = 'b'
   print *, findloc(x, y)     ! { dg-error " must be in type conformance" }
   print *, findloc(x, y, 1)  ! { dg-error " must be in type conformance" }
end


