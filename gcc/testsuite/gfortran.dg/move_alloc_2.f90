! { dg-do run }
!
! PR 45004: [OOP] Segfault with allocatable scalars and move_alloc
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>

program bug18

  type foo
    integer :: i
  end type foo

  type bar
    class(foo), allocatable :: bf
  end type bar

  class(foo), allocatable :: afab
  type(bar) :: bb

  allocate(foo :: afab)
  afab%i = 8
  call move_alloc(afab, bb%bf)
  if (.not. allocated(bb%bf)) call abort()
  if (allocated(afab)) call abort()
  if (bb%bf%i/=8) call abort()

end program bug18
