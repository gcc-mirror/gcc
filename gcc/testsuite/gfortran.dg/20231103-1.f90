! { dg-do compile }
! { dg-options "-Ofast" }
SUBROUTINE sedi_1D(QX1d,  DZ1d,kdir,BX1d,kbot,ktop)
  real, dimension(:) :: QX1d,DZ1d
  real, dimension(size(QX1d))  :: VVQ
  logical                            BX_present
  do k= kbot,ktop,kdir
     VVQ= VV_Q0
  enddo
  Vxmaxx= min0
  if (kdir==1) then
     dzMIN = minval(DZ1d)
  endif
  npassx=   Vxmaxx/dzMIN
  DO nnn= 1,npassx
     if (BX_present) then
       do k= ktop,kdir
         BX1d= iDZ1d0
       enddo
     endif
  ENDDO
END
