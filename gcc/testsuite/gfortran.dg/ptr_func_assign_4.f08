! { dg-do compile }
!
! Tests correction to implementation of pointer function assignments.
!
! Contributed by Mikael Morin  <mikael.morin@sfr.fr>
!
program p
  integer, target :: a(3) = 2
  integer :: b(3, 3) = 1
  integer :: c

  c = 3
  func (b(2, 2)) = b ! { dg-error "Different ranks" }
  func (c) = b       ! { dg-error "Different ranks" }

contains
  function func(arg) result(r)
    integer, pointer :: r(:)
    integer :: arg

    if (arg == 1) then
      r => a
    else
      r => null()
    end if
  end function func
end program p
