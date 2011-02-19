! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Different CHARACTER lengths" }
! PR fortran/33254: No bounds checking for array constructors
program array_char
implicit none
character (len=2) :: x, y
character (len=2) :: z(3)
x = "a "
y = "cd"
z = [y(1:1), y(1:1), x(1:len(trim(x)))]  ! should work
z = [trim(x), trim(y), "aaaa"] ! [ "a", "cd", "aaaa" ] should catch first error
end program array_char

! { dg-output "Different CHARACTER lengths .1/.. in array constructor" }
