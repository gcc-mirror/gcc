! { dg-do run }
! Test the fix for pr69011, preventing an ICE and making sure
! that the correct dynamic type is used.
!
! Contributed by Thomas Koenig  <tkoenig@gcc.gnu.org>
!                Andre Vehreschild  <vehre@gcc.gnu.org>
!
 
module m1
implicit none
private
public :: basetype

type:: basetype
  integer :: i
  contains
endtype basetype

abstract interface
endinterface

endmodule m1

module m2
use m1, only : basetype
implicit none
integer, parameter :: I_P = 4

private
public :: factory, exttype

type, extends(basetype) :: exttype
  integer :: i2
  contains
endtype exttype

type :: factory
  integer(I_P) :: steps=-1 
  contains
    procedure, pass(self), public :: construct
endtype factory
contains

  function construct(self, previous)
  class(basetype), intent(INOUT) :: previous(1:)
  class(factory), intent(IN) :: self
  class(basetype), pointer :: construct
  allocate(construct, source=previous(self%steps))
  endfunction construct
endmodule m2

  use m2
  use m1
  class(factory), allocatable :: c1
  class(exttype), allocatable :: prev(:)
  class(basetype), pointer :: d

  allocate(c1)
  allocate(prev(2))
  prev(:)%i = [ 2, 3]
  prev(:)%i2 = [ 5, 6]
  c1%steps= 1
  d=> c1%construct(prev)

  if (.not. associated(d) ) STOP 1
  select type (d)
    class is (exttype)
      if (d%i2 /= 5) STOP 2
    class default
      STOP 3
  end select 
  if (d%i /= 2) STOP 4
  deallocate(c1)
  deallocate(prev)
  deallocate(d)
end
