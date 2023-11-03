! { dg-do compile }
! { dg-options "-Ofast" }
subroutine shr_map_checkFldStrshr_map_mapSet_dest(ndst,max0,eps,sum0,maxval0,min0,nidnjd,renorm)
  allocatable  sum(:)
  logical renorm
  allocate(sum(ndst))
  do n=1,ndst 
    if (sum0 > eps) then
      rmax = max0
    endif
  enddo
  if (renorm) then
    rmin = maxval0
    rmax = minval(sum)
    do n=1,nidnjd
      if (sum0 > eps) then
        rmin = min0
      endif
    enddo
    write(*,*) rmin,rmax
  endif
end
