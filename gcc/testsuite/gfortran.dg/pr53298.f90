! { dg-do run }

program test
  character(len=5) :: str(3)
  str = ["abcde", "12345", "ABCDE" ]
  call f(str(:))
contains
  subroutine f(x)
    character(len=*) :: x(:)
    write(*,*) x(:)(1:) 
  end subroutine f
end program test

! { dg-output "abcde12345ABCDE" }
