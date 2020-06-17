! { dg-do compile }
! { dg-options "-std=f2003" }
! Based on PR fortran/66724, also covers fortran/66725 and fortran/87923.
!

write (1, blank='') ! { dg-error "BLANK specifier in WRITE statement at ... has invalid value" }

write (1, asynchronous=1) ! { dg-error "ASYNCHRONOUS tag at ... must be of type CHARACTER" }
write (1, asynchronous=1e1) ! { dg-error "ASYNCHRONOUS tag at ... must be of type CHARACTER" }
write (1, asynchronous=1d1) ! { dg-error "ASYNCHRONOUS tag at ... must be of type CHARACTER" }
write (1, asynchronous=.false.) ! { dg-error "ASYNCHRONOUS tag at ... must be of type CHARACTER" }
write (1, asynchronous='') ! { dg-error "ASYNCHRONOUS specifier in WRITE statement at ... has invalid value" }
write (1, asynchronous='no')
write (1, asynchronous=null()) ! { dg-error "ASYNCHRONOUS tag at ... must be of type CHARACTER" }
write (1, asynchronous=(1)) ! { dg-error "ASYNCHRONOUS tag at ... must be of type CHARACTER" }
write (1, asynchronous=(1., 0.)) ! { dg-error "ASYNCHRONOUS tag at ... must be of type CHARACTER" }
write (1, asynchronous=[1]) ! { dg-error "ASYNCHRONOUS tag at ... must be of type CHARACTER" }
write (1, asynchronous=['']) ! { dg-error "ASYNCHRONOUS tag at ... must be scalar" }

write (1, blank=1) ! { dg-error "BLANK tag at ... must be of type CHARACTER" }
write (1, blank=1e1) ! { dg-error "BLANK tag at ... must be of type CHARACTER" }
write (1, blank=1d1) ! { dg-error "BLANK tag at ... must be of type CHARACTER" }
write (1, blank=.false.) ! { dg-error "BLANK tag at ... must be of type CHARACTER" }
write (1, blank='no') ! { dg-error "BLANK specifier in WRITE statement at ... has invalid value" }
write (1, blank=null()) ! { dg-error "BLANK tag at ... must be of type CHARACTER" }
write (1, blank=(1)) ! { dg-error "BLANK tag at ... must be of type CHARACTER" }
write (1, blank=(1., 0.)) ! { dg-error "BLANK tag at ... must be of type CHARACTER" }
write (1, blank=[1]) ! { dg-error "BLANK tag at ... must be of type CHARACTER" }
write (1, blank=['']) ! { dg-error "BLANK tag at ... must be scalar" }

write (1, delim=1) ! { dg-error "DELIM tag at ... must be of type CHARACTER" }
write (1, delim=1e1) ! { dg-error "DELIM tag at ... must be of type CHARACTER" }
write (1, delim=1d1) ! { dg-error "DELIM tag at ... must be of type CHARACTER" }
write (1, delim=.false.) ! { dg-error "DELIM tag at ... must be of type CHARACTER" }
write (1, delim='') ! { dg-error "DELIM specifier in WRITE statement at ... has invalid value" }
write (1, delim='no') ! { dg-error "DELIM specifier in WRITE statement at ... has invalid value" }
write (1, delim=null()) ! { dg-error "DELIM tag at ... must be of type CHARACTER" }
write (1, delim=(1)) ! { dg-error "DELIM tag at ... must be of type CHARACTER" }
write (1, delim=(1., 0.)) ! { dg-error "DELIM tag at ... must be of type CHARACTER" }
write (1, delim=[1]) ! { dg-error "DELIM tag at ... must be of type CHARACTER" }
write (1, delim=['']) ! { dg-error "DELIM tag at ... must be scalar" }

write (1, decimal=1) ! { dg-error "DECIMAL tag at ... must be of type CHARACTER" }
write (1, decimal=1e1) ! { dg-error "DECIMAL tag at ... must be of type CHARACTER" }
write (1, decimal=1d1) ! { dg-error "DECIMAL tag at ... must be of type CHARACTER" }
write (1, decimal=.false.) ! { dg-error "DECIMAL tag at ... must be of type CHARACTER" }
write (1, decimal='') ! { dg-error "DECIMAL specifier in WRITE statement at ... has invalid value" }
write (1, decimal='no') ! { dg-error "DECIMAL specifier in WRITE statement at ... has invalid value" }
write (1, decimal=null()) ! { dg-error "DECIMAL tag at ... must be of type CHARACTER" }
write (1, decimal=(1)) ! { dg-error "DECIMAL tag at ... must be of type CHARACTER" }
write (1, decimal=(1., 0.)) ! { dg-error "DECIMAL tag at ... must be of type CHARACTER" }
write (1, decimal=[1]) ! { dg-error "DECIMAL tag at ... must be of type CHARACTER" }
write (1, decimal=['']) ! { dg-error "DECIMAL tag at ... must be scalar" }

write (1, iomsg=1) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
write (1, iomsg=1e1) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
write (1, iomsg=1d1) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
write (1, iomsg=.false.) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
write (1, iomsg='') ! { dg-error "Non-variable expression" }
write (1, iomsg='no') ! { dg-error "Non-variable expression" }
write (1, iomsg=null()) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
write (1, iomsg=(1)) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
write (1, iomsg=(1., 0.)) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
write (1, iomsg=[1]) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
write (1, iomsg=['']) ! { dg-error "IOMSG tag at ... must be scalar" }

write (1, pad=1) ! { dg-error "PAD tag at ... must be of type CHARACTER" }
write (1, pad=1e1) ! { dg-error "PAD tag at ... must be of type CHARACTER" }
write (1, pad=1d1) ! { dg-error "PAD tag at ... must be of type CHARACTER" }
write (1, pad=.false.) ! { dg-error "PAD tag at ... must be of type CHARACTER" }
write (1, pad='') ! { dg-error "PAD specifier in WRITE statement at ... has invalid value" }
write (1, pad='no') ! { dg-error "the PAD= specifier at ... must be with an explicit format expression" }
write (1, pad=null()) ! { dg-error "PAD tag at ... must be of type CHARACTER" }
write (1, pad=(1)) ! { dg-error "PAD tag at ... must be of type CHARACTER" }
write (1, pad=(1., 0.)) ! { dg-error "PAD tag at ... must be of type CHARACTER" }
write (1, pad=[1]) ! { dg-error "PAD tag at ... must be of type CHARACTER" }
write (1, pad=['']) ! { dg-error "PAD tag at ... must be scalar" }

write (1, round=1) ! { dg-error "ROUND tag at ... must be of type CHARACTER" }
write (1, round=1e1) ! { dg-error "ROUND tag at ... must be of type CHARACTER" }
write (1, round=1d1) ! { dg-error "ROUND tag at ... must be of type CHARACTER" }
write (1, round=.false.) ! { dg-error "ROUND tag at ... must be of type CHARACTER" }
write (1, round='') ! { dg-error "ROUND specifier in WRITE statement at ... has invalid value" }
write (1, round='no') ! { dg-error "ROUND specifier in WRITE statement at ... has invalid value" }
write (1, round=null()) ! { dg-error "ROUND tag at ... must be of type CHARACTER" }
write (1, round=(1)) ! { dg-error "ROUND tag at ... must be of type CHARACTER" }
write (1, round=(1., 0.)) ! { dg-error "ROUND tag at ... must be of type CHARACTER" }
write (1, round=[1]) ! { dg-error "ROUND tag at ... must be of type CHARACTER" }
write (1, round=['']) ! { dg-error "ROUND tag at ... must be scalar" }

write (1, sign=1) ! { dg-error "SIGN tag at ... must be of type CHARACTER" }
write (1, sign=1e1) ! { dg-error "SIGN tag at ... must be of type CHARACTER" }
write (1, sign=1d1) ! { dg-error "SIGN tag at ... must be of type CHARACTER" }
write (1, sign=.false.) ! { dg-error "SIGN tag at ... must be of type CHARACTER" }
write (1, sign='') ! { dg-error "SIGN specifier in WRITE statement at ... has invalid value" }
write (1, sign='no') ! { dg-error "SIGN specifier in WRITE statement at ... has invalid value" }
write (1, sign=null()) ! { dg-error "SIGN tag at ... must be of type CHARACTER" }
write (1, sign=(1)) ! { dg-error "SIGN tag at ... must be of type CHARACTER" }
write (1, sign=(1., 0.)) ! { dg-error "SIGN tag at ... must be of type CHARACTER" }
write (1, sign=[1]) ! { dg-error "SIGN tag at ... must be of type CHARACTER" }
write (1, sign=['']) ! { dg-error "SIGN tag at ... must be scalar" }

end
