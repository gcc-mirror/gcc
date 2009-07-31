program NF
end program NF
subroutine mattest(nx,ny,nz,band1,band2,band3,stiffness,maxiter,targrms,method)
  integer,parameter :: dpkind=kind(1.0D0)
  character(*) :: method
  real(dpkind),allocatable,dimension(:) :: ad,au1,au2,au3,x,b
  allocate(ad(nxyz),au1(nxyz),au2(nxyz),au3(nxyz),x(nxyz),b(nxyz))
  au1(nx:nxyz:nx) = 0.0
  if ( method=='NFCG' ) then                                    
  endif
end subroutine mattest
