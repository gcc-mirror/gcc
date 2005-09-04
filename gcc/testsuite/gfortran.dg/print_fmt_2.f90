! { dg-do compile }
! PR 23661 Make sure space between PRINT and variable name is enforced in
! free form.
! Also tests the namelist case
character(5) :: f = "(a)"
real  x
namelist /mynml/ x
printf, "check" ! { dg-error "Unclassifiable" }
x = 1
printmynml ! { dg-error "" }
end
