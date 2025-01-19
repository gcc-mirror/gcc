! { dg-do compile } 
! { dg-additional-options "-std=f2008 -fcoarray=single" }

module test
  implicit none
contains
  elemental subroutine test1
    !$acc parallel ! { dg-error "may not appear in PURE procedures" }
    !$acc serial ! { dg-error "may not appear in PURE procedures" }
  end subroutine test1

  pure subroutine test2
    !$acc parallel ! { dg-error "may not appear in PURE procedures" }
    !$acc serial ! { dg-error "may not appear in PURE procedures" }
  end subroutine test2

  ! Implicit pure
  elemental real function test3(x)
    real, intent(in) :: x
    !$acc parallel ! { dg-error "may not appear in PURE procedures" }
    !$acc serial ! { dg-error "may not appear in PURE procedures" }
    test3 = x*x
  end function test3

  pure real function test4(x)
    real, intent(in) :: x
    !$acc parallel ! { dg-error "may not appear in PURE procedures" }
    !$acc serial ! { dg-error "may not appear in PURE procedures" }
    test4 = x
  end function test4

  subroutine test5
    real :: x = 0.0
    integer :: i
    !$acc parallel loop collapse(1) reduction(+:x)
    do i = 1,10
      x = x + 0.3
    enddo
    !$acc serial loop collapse(1) reduction(+:x)
    do i = 1,10
      x = x + 0.3
    enddo
    print *, x
  end subroutine test5

  real function test6(x)
    real :: x
    integer :: i
    !$acc parallel loop collapse(1) reduction(+:x)
    do i = 1,10
      x = x + 0.3
    enddo
    !$acc serial loop collapse(1) reduction(+:x)
    do i = 1,10
      x = x + 0.3
    enddo
    test6 = x
  end function test6

  impure elemental real function test7(x)
    real, intent(in) :: x
    !$acc parallel 
    test7 = x
    !$acc end parallel
    !$acc serial 
    test7 = x
    !$acc end serial
  end function test7

  subroutine test8
    real :: x = 0.0
    integer :: i
    !$acc parallel loop collapse(1) reduction(+:x)
    do i = 1,10
      critical ! { dg-error "CRITICAL block inside of" }
        x = x + 0.3
      end critical
    enddo
    !$acc serial loop collapse(1) reduction(+:x)
    do i = 1,10
      critical ! { dg-error "CRITICAL block inside of" }
        x = x + 0.3
      end critical
    enddo
    print *, x
  end subroutine test8

  real function test9(n)
    integer, value :: n
    BLOCK
      integer i
      real sum
      !$acc loop reduction(+:sum)
      do i=1, n
        sum = sum + sin(real(i))
      end do
    END BLOCK
  end function test9
end module test