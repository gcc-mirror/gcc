! { dg-lto-do link }
!
! PR fortran/126170
! A redundant, already host-associated USE inside a contained function
! caused gfortran to create duplicate of a derived type with
! a FINAL binding.

module vsm
  type :: vs
     integer :: c
   contains
     final :: vdst
  end type vs
contains
  subroutine vdst(v)
    type(vs), intent(inout) :: v
  end subroutine vdst
end module vsm

module m_ds
  use vsm
  type :: mdcc
  end type mdcc
contains
  function mdot(self)
    use vsm
    class(mdcc), intent(inout) :: self
    type(vs) :: mdot
    mdot%c = 1
  end function mdot
end module m_ds
