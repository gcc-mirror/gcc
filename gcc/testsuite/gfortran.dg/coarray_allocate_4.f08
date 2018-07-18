! { dg-do run }
! { dg-options "-fcoarray=single" }
!
! Contributed by Gerhard Steinmetz  <gerhard.steinmetz.fortran@t-online.de>
!               Andre Vehreschild <vehre@gcc.gnu.org>
! Check that PR fortran/69451 is fixed.

program main

implicit none

type foo
end type

class(foo), allocatable :: p[:]
class(foo), pointer :: r
class(*), allocatable, target :: z

allocate(p[*])

call s(p, z)
select type (z)
  class is (foo) 
        r => z
  class default
     STOP 1
end select

if (.not. associated(r)) STOP 2

deallocate(r)
deallocate(p)

contains

subroutine s(x, z) 
   class(*) :: x[*]
   class(*), allocatable:: z
   allocate (z, source=x)
end

end

