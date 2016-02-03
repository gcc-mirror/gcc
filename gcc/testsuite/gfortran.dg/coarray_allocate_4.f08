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
     call abort()
end select

if (.not. associated(r)) call abort()

deallocate(r)
deallocate(p)

contains

subroutine s(x, z) 
   class(*) :: x[*]
   class(*), allocatable:: z
   allocate (z, source=x)
end

end

