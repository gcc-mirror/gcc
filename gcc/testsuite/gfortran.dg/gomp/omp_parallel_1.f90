! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/66549
! The resolution of CVN in the middle CLWF's OpenMP construct was
! making the DO loop (wrongly) interpreted as an OpenMP-managed loop, leading
! to an ICE.
!
! Contributed by Andrew Benson <abensonca@gmail.com>.

module smfa
  type :: sgc
   contains
     procedure :: sla => sa
  end type sgc
  class(sgc), pointer :: sg_
  double precision, allocatable, dimension(:) :: vni 
contains
  double precision function sa(self,i)
    class(sgc), intent(in   ) :: self
  end function sa
  subroutine cvn(sg_,vn)
    class(sgc), intent(inout) :: sg_
    double precision, intent(  out), dimension(:) :: vn
    integer :: i
    do i=1,2
       vn(i)= sg_%sla(i)
    end do
  end subroutine cvn
  subroutine clwf()
    !$omp parallel
    call cvn(sg_,vni)
    !$omp end parallel
  end subroutine clwf
end module smfa

! { dg-final { scan-tree-dump-times "#pragma\\s+omp\\s+parallel\\n" 1 "original" } }
