! { dg-do compile }
module test_equivalence
  real, private :: array1(100)
  real, private :: array2(100)
  equivalence(array1,array2)
end module test_equivalence

module mymodule
  use test_equivalence
  real, dimension(:), allocatable :: array1
end module mymodule

program test
  use mymodule
end program test
