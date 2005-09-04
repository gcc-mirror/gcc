! { dg-do run }
! PR 23661
! PRINT with a character format was broken
character(5) :: f = "(a)"
! { dg-output "check" }
print f, "check"
end
