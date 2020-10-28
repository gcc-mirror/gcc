! { dg-do compile }
! PR fortran/97390
!
integer :: tempRbuffer, array, compactHaloInfo, dimsizes, nHaloLayers, gpu_nList_send, gpu_idx_send, gpu_bufferOffset_send, counter
            !$acc data present(tempRbuffer, array, compactHaloInfo, dimsizes, nHaloLayers, gpu_nList_send, gpu_idx_send, gpu_bufferOffset_send) async(counter+1)  ! { dg-error "Line truncated" }
! { dg-error "Syntax error in Open.* variable list" "" { target "*-*-*" } .-1 }

            !$acc end data  ! { dg-error "Unexpected !.ACC END DATA statement" }
end

! { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 }
