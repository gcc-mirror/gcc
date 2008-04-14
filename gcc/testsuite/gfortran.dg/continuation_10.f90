! { dg-do compile }
! { dg-options -std=f95 }
! PR35882 Miscounted continuation lines when interspersed with data
program test_mod
   implicit none

   integer, dimension(50) :: array

   array = 1

   print "(a, i8)", &
      "Line 1", &
      array(2), &
      "Line 3", &
      array(4), &
      "Line 5", &
      array(6), &
      "Line 7", &
      array(8), &
      "Line 9", &
      array(10), &
      "Line 11", &
      array(12), &
      "Line 13", &
      array(14), &
      "Line 15", &
      array(16), &
      "Line 17", &
      array(18), &
      "Line 19", &
      array(20), &
      "Line 21", &
      array(22), &
      "Line 23", &
      array(24), &
      "Line 25", &
      array(26), &
      "Line 27", &
      array(28), &
      "Line 29", &
      array(30), &
      "Line 31", &
      array(32), &
      "Line 33", &
      array(34), &
      "Line 35", &
      array(36), &
      "Line 37", &
      array(38), &
      "Line 39", &
      array(40), & ! { dg-warning "Limit of 39 continuations exceeded" }
      "Line 41", &
      array(42), &
      "Line 43"
end program
