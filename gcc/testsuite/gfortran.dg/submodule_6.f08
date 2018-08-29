! { dg-do run }
! { dg-require-effective-target lto }
! { dg-options "-flto" }
!
! Checks that the results of module procedures have the correct characteristics
! and that submodules use the module version of vtables (PR66762). This latter
! requires the -flto compile option.
!
! Contributed by Reinhold Bader  <reinhold.bader@lrz.de>
!
module mod_a
  implicit none
  type, abstract :: t_a
  end type t_a
  interface
    module subroutine p_a(this, q)
      class(t_a), intent(inout) :: this
      class(*), intent(in) :: q
    end subroutine
    module function create_a() result(r)
      class(t_a), allocatable :: r
    end function
    module subroutine print(this)
      class(t_a), intent(in) :: this
    end subroutine
  end interface
end module mod_a

module mod_b
  implicit none
  type t_b
    integer, allocatable :: I(:)
  end type t_b
  interface
    module function create_b(i) result(r)
      type(t_b) :: r
      integer :: i(:)
    end function
  end interface
end module mod_b

submodule(mod_b) imp_create
contains
  module procedure create_b
    if (allocated(r%i)) deallocate(r%i)
    allocate(r%i, source=i)
  end procedure
end submodule imp_create

submodule(mod_a) imp_p_a
  use mod_b
  type, extends(t_a) :: t_imp
    type(t_b) :: b
  end type t_imp
  integer, parameter :: ii(2) = [1,2]
contains
  module procedure create_a
    type(t_b) :: b
    b = create_b(ii)
    allocate(r, source=t_imp(b))
  end procedure

  module procedure  p_a
    select type (this)
      type is (t_imp)
        select type (q)
          type is (t_b)
            this%b = q
          class default
            STOP 1
         end select
      class default
        STOP 2
      end select
  end procedure p_a
  module procedure print
    select type (this)
      type is (t_imp)
        if (any (this%b%i .ne. [3,4,5])) STOP 3
      class default
        STOP 4
    end select
  end procedure
end submodule imp_p_a

program p
  use mod_a
  use mod_b
  implicit none
  class(t_a), allocatable :: a
  allocate(a, source=create_a())
  call p_a(a, create_b([3,4,5]))
  call print(a)
end program p
