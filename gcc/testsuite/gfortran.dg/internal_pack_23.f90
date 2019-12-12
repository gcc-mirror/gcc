! { dg-do run }
! PR fortran/90539 - this used to cause an ICE.

module t2
  implicit none
contains
  subroutine foo(a)
    real, dimension(*) :: a
    if (a(1) /= 1.0 .or. a(2) /= 2.0) stop 1
  end subroutine foo
end module t2

module t1
  use t2
  implicit none
contains
  subroutine bar(a)
    real, dimension(:) :: a
    if (a(1) /= 1.0 .or. a(2) /= 2.0) stop 1
    call foo(a)
  end subroutine bar
end module t1

program main
  use t1
  call bar([1.0, 2.0])
end program main
