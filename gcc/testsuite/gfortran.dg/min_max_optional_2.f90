! { dg-do run }
! { dg-shouldfail "" }
  program test 
    if (m1(3,4) /= 4) call abort
    if (m1(3) /= 3) call abort
    print *, m1() 
  contains 
    integer function m1(a1,a2) 
      integer, optional :: a1,a2 
      m1 = max(a2, a1, 1, 2) 
    end function m1 
  end 
! { dg-output "First argument of 'max' intrinsic should be present" }
