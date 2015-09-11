! { dg-do compile }
! { dg-options "-O3 -fno-ipa-cp -fdump-ipa-inline" }

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

! { dg-final { scan-ipa-dump-not "redefined extern inline functions are not considered for inlining" "inline" } }
