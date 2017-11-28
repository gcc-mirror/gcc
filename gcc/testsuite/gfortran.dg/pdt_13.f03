! { dg-do run }
!
! Test the fix for PR82375
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

  type link(real_kind)
    integer, kind :: real_kind
    real (kind=real_kind) :: n
    type (link(real_kind)), pointer :: next => NULL()
  end type link

contains

  function push_8 (self, arg) result(current)
    real(dp) :: arg
    type (link(real_kind=dp)), pointer :: self
    type (link(real_kind=dp)), pointer :: current

    if (associated (self)) then
      current => self
      do while (associated (current%next))
        current => current%next
      end do

      allocate (current%next)
      current => current%next
    else
      allocate (current)
      self => current
    end if

    current%n = arg
    current%next => NULL ()
  end function push_8

  function pop_8 (self) result(res)
    type (link(real_kind=dp)), pointer :: self
    type (link(real_kind=dp)), pointer :: current => NULL()
    type (link(real_kind=dp)), pointer :: previous => NULL()
    real(dp) :: res

    res = 0.0_8
    if (associated (self)) then
      current => self
      do while (associated (current) .and. associated (current%next))
         previous => current
         current => current%next
      end do

      previous%next => NULL ()

      res = current%n
      if (associated (self, current)) then
        deallocate (self)
      else
        deallocate (current)
      end if

    end if
  end function pop_8

end module link_module

program ch2701
  use precision_module
  use link_module
  implicit none
  integer, parameter :: wp = dp
  type (link(real_kind=wp)), pointer :: root => NULL()
  type (link(real_kind=wp)), pointer :: current

  current => push_8 (root, 1.0_8)
  current => push_8 (root, 2.0_8)
  current => push_8 (root, 3.0_8)

  if (int (pop_8 (root)) .ne. 3) call abort
  if (int (pop_8 (root)) .ne. 2) call abort
  if (int (pop_8 (root)) .ne. 1) call abort
  if (int (pop_8 (root)) .ne. 0) call abort

end program ch2701
