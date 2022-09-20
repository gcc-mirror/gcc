! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR82375. This is a wrinkle on the allocatable
! version of pdt_13.f03, pdt_14.f03, whereby 'root' is now declared
! in a subroutine so that it should be cleaned up automatically. This
! is best tested with valgrind or its like.
! In addition, the field 'n' has now become a parameterized length
! array to verify that the combination of allocatable components and
! parameterization works correctly.
!
! Based on contribution by Ian Chivers  <ian@rhymneyconsulting.co.uk>
!
module precision_module
  implicit none
  integer, parameter :: sp = selected_real_kind(6, 37)
  integer, parameter :: dp = selected_real_kind(15, 307)
  integer, parameter :: qp = selected_real_kind( 30, 291)
end module precision_module

module link_module
  use precision_module

  type link(real_kind, mat_len)
    integer, kind :: real_kind
    integer, len :: mat_len
    real (kind=real_kind) :: n(mat_len)
    type (link(real_kind, :)), allocatable :: next
  end type link

contains

  function push_8 (self, arg) result(current)
    real(dp) :: arg
    type (link(real_kind=dp, mat_len=:)), allocatable, target :: self
    type (link(real_kind=dp, mat_len=:)), pointer :: current

    if (allocated (self)) then
      current => self
      do while (allocated (current%next))
        current => current%next
      end do

      allocate (link(real_kind=dp, mat_len=1) :: current%next)
      current => current%next
    else
      allocate (link(real_kind=dp, mat_len=1) :: self)
      current => self
    end if

    current%n(1) = arg

  end function push_8

  function pop_8 (self) result(res)
    type (link(real_kind=dp, mat_len=:)), allocatable, target :: self
    type (link(real_kind=dp, mat_len=:)), pointer:: current => NULL()
    type (link(real_kind=dp, mat_len=:)), pointer :: previous => NULL()
    real(dp) :: res

    res = 0.0_8
    if (allocated (self)) then
      current => self
      previous => self
      do while (allocated (current%next))
         previous => current
         current => current%next
      end do
      res = current%n(1)
      if (.not.allocated (previous%next)) then
        deallocate (self)
      else
        deallocate (previous%next)
      end if

    end if
  end function pop_8

end module link_module

program ch2701
  use precision_module
  use link_module
  implicit none
  integer, parameter :: wp = dp

  call foo
contains

  subroutine foo
    type (link(real_kind=wp, mat_len=:)), allocatable :: root
    type (link(real_kind=wp, mat_len=:)), pointer :: current => NULL()

    current => push_8 (root, 1.0_8)
    current => push_8 (root, 2.0_8)
    current => push_8 (root, 3.0_8)

    if (int (pop_8 (root)) .ne. 3) STOP 1
    if (int (pop_8 (root)) .ne. 2) STOP 2
    if (int (pop_8 (root)) .ne. 1) STOP 3
!    if (int (pop_8 (root)) .ne. 0) STOP 4
  end subroutine
end program ch2701
! { dg-final { scan-tree-dump-times "Pdtlink_8._deallocate " 5 "original" } }
! { dg-final { scan-tree-dump-times ".n.data = 0B" 8 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free" 14 "original" } }
