! { dg-do run }
program value_tests_f03
  use, intrinsic :: iso_c_binding
  real(c_double) :: myDouble
  interface
     subroutine value_test(myDouble) bind(c)
       use, intrinsic :: iso_c_binding
       real(c_double), value :: myDouble
     end subroutine value_test
  end interface

  myDouble = 9.0d0
  call value_test(myDouble)
end program value_tests_f03

subroutine value_test(myDouble) bind(c)
  use, intrinsic :: iso_c_binding
  real(c_double), value :: myDouble
  interface
     subroutine mySub(myDouble)
       use, intrinsic :: iso_c_binding
       real(c_double), value :: myDouble
     end subroutine mySub
  end interface

  myDouble = 10.0d0

  call mySub(myDouble)
end subroutine value_test

subroutine mySub(myDouble)
  use, intrinsic :: iso_c_binding
  real(c_double), value :: myDouble
  
  myDouble = 11.0d0
end subroutine mySub
  
