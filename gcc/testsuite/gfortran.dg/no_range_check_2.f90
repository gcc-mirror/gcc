! { dg-do run }
! { dg-options "-fno-range-check" }
! PR36515 Integer read a value overflow for an invalid integer.
! This tests that -fno-range-check allows this legacy behavior at runtime.
program int_range
character(25) :: inputline = "-2147483648"
integer*4 smallest
read(inputline,100) smallest
100 format(1i11)
if (smallest.ne.-2147483648) STOP 1
end
