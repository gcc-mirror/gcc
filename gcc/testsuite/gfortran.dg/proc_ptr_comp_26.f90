! { dg-do compile }
!
! PR 46841: [F03] ICE on allocating array of procedure pointers
!
! Contributed by Martien Hulsen <m.a.hulsen@tue.nl>

  type vfunc_p
    procedure (dum_vfunc), pointer, nopass :: p => null()
  end type vfunc_p

  type(vfunc_p), allocatable, dimension(:) :: vfunc1 

  allocate(vfunc1(10))

contains

  function dum_vfunc ()
    real, dimension(2) :: dum_vfunc
    dum_vfunc = 0
  end function dum_vfunc

end 
