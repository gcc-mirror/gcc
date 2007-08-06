! { dg-do run }
! { dg-shouldfail "" }
program test
  call foo("foo")
contains
  subroutine foo(a, b, c, d)
    character(len=*), optional :: a, b, c, d
    integer :: i
    i = len_trim(min(a,b,c,d)) ! { dg-output "Second argument of 'MIN' intrinsic should be present" }
    print *, i
  end subroutine foo
end
