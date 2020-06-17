! { dg-do compile }
! { dg-options "-std=f2003" }
! Based on PR fortran/66724, also covers fortran/66725 and fortran/87923.
!

close (1, iomsg=1) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
close (1, iomsg=1e1) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
close (1, iomsg=1d1) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
close (1, iomsg=.false.) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
close (1, iomsg='') ! { dg-error "Non-variable expression" }
close (1, iomsg='no') ! { dg-error "Non-variable expression" }
close (1, iomsg=null()) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
close (1, iomsg=(1)) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
close (1, iomsg=(1., 0.)) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
close (1, iomsg=[1]) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
close (1, iomsg=['']) ! { dg-error "IOMSG tag at ... must be scalar" }
close (1, iomsg=['no']) ! { dg-error "IOMSG tag at ... must be scalar" }

close (1, status=1) ! { dg-error "STATUS tag at ... must be of type CHARACTER" }
close (1, status=1e1) ! { dg-error "STATUS tag at ... must be of type CHARACTER" }
close (1, status=1d1) ! { dg-error "STATUS tag at ... must be of type CHARACTER" }
close (1, status=.false.) ! { dg-error "STATUS tag at ... must be of type CHARACTER" }
close (1, status='') ! { dg-error "STATUS specifier in CLOSE statement at ... has invalid value" }
close (1, status='no') ! { dg-error "STATUS specifier in CLOSE statement at ... has invalid value" }
close (1, status=null()) ! { dg-error "STATUS tag at ... must be of type CHARACTER" }
close (1, status=(1)) ! { dg-error "STATUS tag at ... must be of type CHARACTER" }
close (1, status=(1., 0.)) ! { dg-error "STATUS tag at ... must be of type CHARACTER" }
close (1, status=[1]) ! { dg-error "STATUS tag at ... must be of type CHARACTER" }
close (1, status=['']) ! { dg-error "STATUS tag at ... must be scalar" }
end
