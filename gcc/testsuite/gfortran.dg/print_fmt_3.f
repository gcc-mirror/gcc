! { dg-do compile }
! PR 23661 Make sure space between PRINT and variable name is not enforced in
! fixed form.
! Also tests the namelist case
      character(5) :: f = "(a)"
      real  x
      namelist /mynml/ x
      printf, "check"
      x = 1
      printmynml ! { dg-warning "extension" }
      end
