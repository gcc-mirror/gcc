! PR rtl-optimization/47008
! { dg-do run }
! { dg-options "-Os -fno-asynchronous-unwind-tables -fschedule-insns -fsched-pressure -fno-inline" { target i?86-*-* x86_64-*-* } }

program main
  type :: t
    integer :: i
    character(24) :: c
    type (t), pointer :: p
  end type t
  type(t), pointer :: r, p
  allocate (p)
  p = t (123455, "", p)
  r => entry ("", 123456, 1, "", 99, "", p)
  if (p%i /= 123455) STOP 1
contains
  function entry (x, i, j, c, k, d, p) result (q)
    integer :: i, j, k
    character (*) :: x, c, d
    type (t), pointer :: p, q
    allocate (q)
    q = t (i, c, p)
  end function
end program main
