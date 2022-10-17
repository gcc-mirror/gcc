! { dg-do run }
! { dg-additional-sources assumed_type_13.c }

use iso_c_binding, only: c_size_t, c_int
implicit none (type, external)

interface
  subroutine test_c (x, n, num) bind (C)
    import :: c_size_t, c_int
    integer(c_size_t), value :: n
    integer(c_int), value :: num
    type(*) :: x(:)
  end subroutine test_c
end interface

complex(8) :: b(3)

call test_c ([1_2, 2_2, 3_2], sizeof(1_2), num=1)
call test_c (b, sizeof(b(1)), num=2)
call outer_bc ([1_2, 2_2, 3_2], sizeof(1_2), num=1)
call outer_bc (b, sizeof(b(1)), num=2)
call outer_f ([1_2, 2_2, 3_2], sizeof(1_2), num=1)
call outer_f (b, sizeof(b(1)), num=2)

contains

subroutine outer_bc (x, n, num) bind(C)
  integer(c_size_t), value :: n
  integer(c_int), value :: num
  type(*) :: x(:)
  !  print *,sizeof(x)/size(x), n
  if (sizeof(x)/size(x) /=  n) error stop 1
  call inner_bc (x, n, num)
  call inner_f (x, n, num)
  call test_c (x, n, num)
end

subroutine outer_f (x, n, num)
  integer(c_size_t), value :: n
  integer(c_int), value :: num
  type(*) :: x(:)
  !  print *,sizeof(x)/size(x), n
  if (sizeof(x)/size(x) /=  n) error stop 1
  call inner_f (x, n, num)
  call inner_bc (x, n, num)
  call test_c (x, n, num)
end

subroutine inner_bc(x, n, num) bind(C)
  integer(c_size_t), value :: n
  integer(c_int), value :: num
  type(*) :: x(:)
  !  print *,sizeof(x)/size(x), n
  if (sizeof(x)/size(x) /=  n) error stop 2
  call test_c (x, n, num)
end

subroutine inner_f(x, n, num)
  integer(c_size_t), value :: n
  integer(c_int), value :: num
  type(*) :: x(:)
  !  print *,sizeof(x)/size(x), n
  if (sizeof(x)/size(x) /=  n) error stop 3
  call test_c (x, n, num)
end
end 
