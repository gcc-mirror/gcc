module solv_cap
  integer,       private, save :: Ng1=0, Ng2=0
contains
  subroutine FourirG(G)
    real, intent(in out), dimension(0:,0:) :: G
    complex, allocatable, dimension(:,:)   :: t
    allocate( t(0:2*Ng1-1,0:2*Ng2-1) )
    t(0:Ng1,0:Ng2-1)    = G(:,0:Ng2-1)      ! Fill one quadrant (one extra row)
    t(0:Ng1,Ng2:2*Ng2-1) = G(:,Ng2:1:-1)    ! This quadrant using symmetry
  end subroutine FourirG
end module solv_cap
