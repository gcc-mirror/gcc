! { dg-do compile }
!
! PR fortran/49690
!
! Reduced test case, based on the one of Debian bug #631204
!

subroutine ctrlc_ast
   common /xinterrupt/ interrupted
   logical interrupted
   interrupted = .true.
end subroutine ctrlc_ast  

subroutine set_ctrl_c(ctrlc_ast)
   external ctrlc_ast
   intrinsic signal
   integer old_handle
   common /xinterrupt/ interrupted
   logical interrupted
   old_handler = signal(2, ctrlc_ast)    
end subroutine set_ctrl_c
