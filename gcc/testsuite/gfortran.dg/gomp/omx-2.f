! { dg-additional-options "-Wsurprising" }

!$omx foo  ! { dg-warning "Ignoring '!.omx' vendor-extension sentinel" }
!$OMX foo  ! { dg-warning "Ignoring '!.omx' vendor-extension sentinel" }
c$oMx foo  ! { dg-warning "Ignoring '!.omx' vendor-extension sentinel" }
c$OMx foo  ! { dg-warning "Ignoring '!.omx' vendor-extension sentinel" }
*$oMx foo  ! { dg-warning "Ignoring '!.omx' vendor-extension sentinel" }
*$OMx foo  ! { dg-warning "Ignoring '!.omx' vendor-extension sentinel" }
      end
