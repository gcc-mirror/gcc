! { dg-do run }
! { dg-options "-O2 -std=legacy -fdump-tree-optimized" }
!
! PR fortran/121203 - fix passing of character length of function to procedure

program p
  character(10), external :: f
  call eval (f,"abc")
  call eval2(f,"abc")
contains
  subroutine eval2(func,c_arg)
    character(*) c_arg
    character(*) func
    external func
    ! These tests should get optimized:
    if (len      (c_arg)  /=  3) stop 1
    if (len (func(c_arg)) /= 10) stop 2
  end subroutine
end

character(10) function f(arg)
  character(*) arg
  f=arg
end

subroutine eval(func,c_arg)
  character(*) c_arg
  character(*) func
  external func
  if (len      (c_arg)  /=  3) error stop 3
  if (len (func(c_arg)) /= 10) error stop 4
end subroutine

! { dg-final { scan-tree-dump-not "_gfortran_stop_numeric" "optimized" } }
