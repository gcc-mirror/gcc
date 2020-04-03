! { dg-do compile }
!
! Test case by  G. Steinmetz

program p
   character(len=1, kind=1) :: x(3) = ['a', 'b', 'c']
   character(len=1, kind=4) :: y = 4_'b'
   print *, findloc(x, y)     ! { dg-error " must be in type conformance" }
   print *, findloc(x, y, 1)  ! { dg-error " must be in type conformance" }
end

