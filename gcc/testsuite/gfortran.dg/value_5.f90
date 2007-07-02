! { dg-do compile }
! Length of character dummy variable with VALUE attribute:
! - must be initialization expression or omitted
! - C interoperable: must be initialization expression of length one
!   or omitted
!
! Contributed by Tobias Burnus
program x
  implicit none
  character(10) :: c1,c10
  c1  = 'H'
  c10 = 'Main'
  call foo1(c1)
  call foo2(c1)
  call foo3(c10)
  call foo4(c10)
  call bar1(c1)
  call bar2(c1)
  call bar3(c10)
  call bar4(c10)

contains

  subroutine foo1(a)
    character :: a
    value :: a
  end subroutine foo1

  subroutine foo2(a)
    character(1) :: a
    value :: a
  end subroutine foo2

  subroutine foo3(a)
    character(10) :: a
    value :: a
  end subroutine foo3

  subroutine foo4(a) ! { dg-error "VALUE attribute must have constant length" }
    character(*) :: a
    value :: a
  end subroutine foo4

  subroutine bar1(a)
    use iso_c_binding, only: c_char
    character(kind=c_char) :: a
    value :: a
  end subroutine bar1

  subroutine bar2(a)
    use iso_c_binding, only: c_char
    !character(kind=c_char,len=1) :: a
    character(1,kind=c_char) :: a
    value :: a
  end subroutine bar2

  subroutine bar3(a) ! { dg-error "VALUE attribute must have length one" }
    use iso_c_binding, only: c_char
    character(kind=c_char,len=10) :: a
    value :: a
  end subroutine bar3

  subroutine bar4(a) ! { dg-error "VALUE attribute must have constant length" }
    use iso_c_binding, only: c_char
    character(kind=c_char,len=*) :: a
    value :: a
  end subroutine bar4
end program x
