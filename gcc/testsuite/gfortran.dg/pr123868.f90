! { dg-do run }
! PR fortran/123868 - Memory leak on assignment with nested allocatable
! components.  Regression introduced by PR121628 commit which caused
! gfc_duplicate_allocatable to be called twice for allocatable array
! components with nested allocatable components.

module bugMod

  type :: vs
     character(len=1), allocatable :: s
  end type vs

  type :: ih
     type(vs), allocatable, dimension(:) :: hk
  end type ih

end module bugMod

program bugProg
  use bugMod

  block
    type(ih) :: c, d

    allocate(d%hk(1))
    allocate(d%hk(1)%s)
    d%hk(1)%s='z'
    c=d
    if (c%hk(1)%s /= 'z') stop 1
    if (d%hk(1)%s /= 'z') stop 2

  end block

end program bugProg
