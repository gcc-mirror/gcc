! { dg-do run }
! Based on attachment to PR82173 comment 10 - reproducers/bug9-class-is-kind-ice.f90
!
! This was straightforward to fix; apart from line 36, which required considerable
! assistance from Claude Haiku  <claude@anthropic.com>.
!
! Contributed by Jerry Delisle  <delisle@gcc.gnu.org>
!
module mj
  type :: base(k, n)
    integer, kind :: k = 4
    integer, len :: n = 1
    real(k) :: v(n)
  end type
end module
program j1
  use mj
  class(*), allocatable :: u
  allocate (base(4, 3) :: u) ! This failed "Double CLASS IS block"
  call selector (u, 3)
  allocate (base(4, 5) :: u) ! n = 3 was returned
  call selector (u, 5)
  call the_rest ()
contains
  subroutine selector (u, sz)
    class(*), allocatable :: u
    integer :: sz
    if (.not.allocated (u)) return
    select type (u)
      class is (base(4, *))     ! distinct kind parameter values are
        if (size (u%v, 1) /= sz) stop 1
        if (kind (u%v) /= 4) stop 2
      class is (base(8, *))     ! distinct guards - C1168 permits this
        if (size(u%v, 1) /= sz) stop 3
        if (kind (u%v) /= 8) stop 4
      class is (base(16, *))    ! Keyword argument ‘k’ at (1) is not in the procedure
        if (size(u%v, 1) /= sz) stop 5
    end select
    if (allocated (u)) deallocate (u)
  end
  subroutine the_rest ()
    allocate (base(8, 5) :: u) ! This failed
    call selector (u, 5)
    allocate (u, source = base(4, 3)([1,2,3]))
    call selector (u, 3)
    allocate (u, mold = base(16, 4)([1.,2.,3.,4.])) ! This failed
    call selector (u, 4)
    u = base(4, 3)([1,2,3])
    call selector (u, 3)
  end
end program
