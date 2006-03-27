! { dg-do compile }
subroutine foo
  character(len=5) :: a
  a = "yes"
  write(*, '(a)', advance=a) "hello world"
end subroutine foo
