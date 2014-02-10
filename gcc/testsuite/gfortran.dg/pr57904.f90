! { dg-do compile }
! { dg-options "-O2" }

program test
  call test2 ()
contains
  subroutine test2 ()
    type t
      integer, allocatable :: x
    end type t

    type t2
      class(t), allocatable :: a
    end type t2

    type(t2) :: one, two

    allocate (two%a)
    one = two
  end subroutine test2
end program test

