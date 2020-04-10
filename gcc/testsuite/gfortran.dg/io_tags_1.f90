! { dg-do compile }
! { dg-options "-std=f2003" }
! Based on PR fortran/66724, also covers fortran/66725 and fortran/87923.
!


backspace (1, iomsg=1) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
backspace (1, iomsg=1e1) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
backspace (1, iomsg=1d1) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
backspace (1, iomsg=.false.) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
backspace (1, iomsg='') ! { dg-error "Non-variable expression" }
backspace (1, iomsg='no') ! { dg-error "Non-variable expression" }
backspace (1, iomsg=null()) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
backspace (1, iomsg=(1)) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
backspace (1, iomsg=(1., 0.)) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
backspace (1, iomsg=[1]) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
backspace (1, iomsg=['']) ! { dg-error "IOMSG tag at ... must be scalar" }
backspace (1, iomsg=['no']) ! { dg-error "IOMSG tag at ... must be scalar" }
end
