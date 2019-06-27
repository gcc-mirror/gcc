! { dg-do compile }
! { dg-options "-O0 -fdump-tree-original" }
!
! PR fortran/49074
! ICE on defined assignment with class arrays.

      module foo
        type bar
          integer :: i

          contains

          generic :: assignment (=) => assgn_bar
          procedure, private :: assgn_bar
        end type bar

        contains

        elemental subroutine assgn_bar (a, b)
          class (bar), intent (inout) :: a
          class (bar), intent (in) :: b

          select type (b)
          type is (bar)
            a%i = b%i
          end select

          return
        end subroutine assgn_bar
      end module foo

      program main
        use foo

        type (bar), allocatable :: foobar(:)

        allocate (foobar(2))
        foobar = [bar(1), bar(2)]
        if (any(foobar%i /= [1, 2])) STOP 1
      end program

! { dg-final { scan-tree-dump-not "_gfortran_internal_pack" "original" } }
! { dg-final { scan-tree-dump-not "_gfortran_internal_unpack" "original" } }
