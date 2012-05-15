! { dg-do run }
! 
! PR fortran/34137
!
! Entry was previously not possible in a module.
! Checks also whether the different result combinations
! work properly.
!
module m1
  implicit none
contains
function func(a)
  implicit none
  integer :: a, func
  real :: ent
  func = a*4
  return
entry ent(a)
  ent = -a*2.0
  return
end function func
end module m1

module m2
  implicit none
contains
function func(a)
  implicit none
  integer :: a, func
  real :: func2
  func = a*8
  return
entry ent(a) result(func2)
  func2 = -a*4.0
  return
end function func
end module m2

module m3
  implicit none
contains
function func(a) result(res)
  implicit none
  integer :: a, res
  real :: func2
  res = a*12
  return
entry ent(a) result(func2)
  func2 = -a*6.0
  return
end function func
end module m3


module m4
  implicit none
contains
function func(a) result(res)
  implicit none
  integer :: a, res
  real :: ent
  res = a*16
  return
entry ent(a)
  ent = -a*8.0
  return
end function func
end module m4

program main
  implicit none
  call test1()
  call test2()
  call test3()
  call test4()
contains
  subroutine test1()
    use m1
    implicit none
    if(func(3) /= 12) call abort()
    if(abs(ent(7) + 14.0) > tiny(1.0)) call abort()
  end subroutine test1
  subroutine test2()
    use m2
    implicit none
    if(func(9) /= 72) call abort()
    if(abs(ent(11) + 44.0) > tiny(1.0)) call abort()
  end subroutine test2
  subroutine test3()
    use m3
    implicit none
    if(func(13) /= 156) call abort()
    if(abs(ent(17) + 102.0) > tiny(1.0)) call abort()
  end subroutine test3
  subroutine test4()
    use m4
    implicit none
    if(func(23) /= 368) call abort()
    if(abs(ent(27) + 216.0) > tiny(1.0)) call abort()
  end subroutine test4
end program main
