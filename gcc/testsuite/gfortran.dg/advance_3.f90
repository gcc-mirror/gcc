subroutine foo
  real :: a
  a = 1
  write(*, '(a)', advance=a) "hello world" ! { dg-error "must be of type CHARACTER" }
end subroutine foo
subroutine bar
  write(*, '(a)', advance=5.) "hello world" ! { dg-error "must be of type CHARACTER" }
end subroutine bar
