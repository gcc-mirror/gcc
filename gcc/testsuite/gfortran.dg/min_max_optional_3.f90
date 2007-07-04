! { dg-do run }
! { dg-shouldfail "" }
  program test 
    if (m1(1,2,3,4) /= 1) call abort
    if (m1(1,2,3) /= 1) call abort
    if (m1(1,2) /= 1) call abort
    print *, m1(1) 
    print *, m1() 
  contains 
    integer function m1(a1,a2,a3,a4) 
      integer, optional :: a1,a2,a3,a4 
      m1 = min(a1,a2,a3,a4) ! { dg-output "Second argument of 'min' intrinsic should be present" }
    end function m1 
  end 
