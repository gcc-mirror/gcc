! { dg-do run }
!
! PR fortran/55763
!
! Contributed by Reinhold Bader
!
module mod_alloc_scalar_01
contains
  subroutine construct(this)
    class(*), allocatable, intent(out) :: this
    integer :: this_i
    this_i = 4
    allocate(this, source=this_i)
  end subroutine
end module

program alloc_scalar_01
  use mod_alloc_scalar_01
  implicit none
  class(*), allocatable :: mystuff

  call construct(mystuff)
  call construct(mystuff)

  select type(mystuff)
  type is (integer)
    if (mystuff == 4) then
!      write(*,*) 'OK'
    else 
      STOP 1
!     write(*,*) 'FAIL 1'
    end if
  class default
    STOP 2
!    write(*,*) 'FAIL 2'
  end select
end program
