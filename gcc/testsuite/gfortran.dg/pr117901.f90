! { dg-do compile }
! { dg-options "-O3" }
!
! Test the fix for pr117901, in which the variable length character in
! the SELECT TYPE construct caused an ICE in make_ssa_name_fn. This is
! a much reduced testcase, extracted from class_transformational_1.f90.
! Note that it does not have references to transformational functions
! of class objects!
!
Module class_tests
contains
  subroutine class_rebar (arg)
    class(*), allocatable :: arg(:)
    call class_bar (arg)
  end
  subroutine class_bar(x)
    class(*), intent(in) :: x(..)
    integer :: checksum
    select rank (x)
      rank (1)
        select type (x)
          type is (character(*))
            checksum = sum(ichar(x(:)(1:1)) + ichar(x(:)(2:2)))
            print *, checksum
        end select
      rank (2)
      rank (3)
      end select
  end
end module class_tests
