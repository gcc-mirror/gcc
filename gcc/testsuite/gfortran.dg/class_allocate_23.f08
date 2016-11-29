! { dg-do run }
!
! Test that pr78356 is fixed.
! Contributed by Janus Weil and Andrew Benson

program p
  implicit none
  type ac
  end type
  type, extends(ac) :: a
     integer, allocatable :: b
  end type
  type n
     class(ac), allocatable :: acr(:)
  end type
  type(n) :: s,t
  allocate(a :: s%acr(1))
  call nncp(s,t)
  select type (cl => t%acr(1))
    class is (a)
      if (allocated(cl%b)) error stop
    class default
      error stop
  end select
contains
  subroutine nncp(self,tg)
    type(n) :: self, tg
    allocate(tg%acr(1),source=self%acr(1))
  end
end

