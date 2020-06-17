! { dg-do compile }
! { dg-options "-std=f2003" }
! Based on PR fortran/66724, also covers fortran/66725 and fortran/87923.
!

open (1, access=1) ! { dg-error "ACCESS tag at ... must be of type CHARACTER" }
open (1, access=1e1) ! { dg-error "ACCESS tag at ... must be of type CHARACTER" }
open (1, access=1d1) ! { dg-error "ACCESS tag at ... must be of type CHARACTER" }
open (1, access=.false.) ! { dg-error "ACCESS tag at ... must be of type CHARACTER" }
open (1, access='') ! { dg-error "ACCESS specifier in OPEN statement at ... has invalid value" }
open (1, access='no') ! { dg-error "ACCESS specifier in OPEN statement at ... has invalid value" }
open (1, access=null()) ! { dg-error "ACCESS tag at ... must be of type CHARACTER" }
open (1, access=(1)) ! { dg-error "ACCESS tag at ... must be of type CHARACTER" }
open (1, access=(1., 0.)) ! { dg-error "ACCESS tag at ... must be of type CHARACTER" }
open (1, access=[1]) ! { dg-error "ACCESS tag at ... must be of type CHARACTER" }
open (1, access=['']) ! { dg-error "ACCESS tag at ... must be scalar" }

open (1, action=1) ! { dg-error "ACTION tag at ... must be of type CHARACTER" }
open (1, action=1e1) ! { dg-error "ACTION tag at ... must be of type CHARACTER" }
open (1, action=1d1) ! { dg-error "ACTION tag at ... must be of type CHARACTER" }
open (1, action=.false.) ! { dg-error "ACTION tag at ... must be of type CHARACTER" }
open (1, action='') ! { dg-error "ACTION specifier in OPEN statement at ... has invalid value" }
open (1, action='no') ! { dg-error "ACTION specifier in OPEN statement at ... has invalid value" }
open (1, action=null()) ! { dg-error "ACTION tag at ... must be of type CHARACTER" }
open (1, action=(1)) ! { dg-error "ACTION tag at ... must be of type CHARACTER" }
open (1, action=(1., 0.)) ! { dg-error "ACTION tag at ... must be of type CHARACTER" }
open (1, action=[1]) ! { dg-error "ACTION tag at ... must be of type CHARACTER" }
open (1, action=['']) ! { dg-error "ACTION tag at ... must be scalar" }

open (1, asynchronous=1) ! { dg-error "ASYNCHRONOUS tag at ... must be of type CHARACTER" }
open (1, asynchronous=1e1) ! { dg-error "ASYNCHRONOUS tag at ... must be of type CHARACTER" }
open (1, asynchronous=1d1) ! { dg-error "ASYNCHRONOUS tag at ... must be of type CHARACTER" }
open (1, asynchronous=.false.) ! { dg-error "ASYNCHRONOUS tag at ... must be of type CHARACTER" }
open (1, asynchronous='') ! { dg-error "ASYNCHRONOUS specifier in OPEN statement at ... has invalid value" }
open (1, asynchronous='no')
open (1, asynchronous=null()) ! { dg-error "ASYNCHRONOUS tag at ... must be of type CHARACTER" }
open (1, asynchronous=(1)) ! { dg-error "ASYNCHRONOUS tag at ... must be of type CHARACTER" }
open (1, asynchronous=(1., 0.)) ! { dg-error "ASYNCHRONOUS tag at ... must be of type CHARACTER" }
open (1, asynchronous=[1]) ! { dg-error "ASYNCHRONOUS tag at ... must be of type CHARACTER" }
open (1, asynchronous=['']) ! { dg-error "ASYNCHRONOUS tag at ... must be scalar" }

open (1, blank=1) ! { dg-error "BLANK tag at ... must be of type CHARACTER" }
open (1, blank=1e1) ! { dg-error "BLANK tag at ... must be of type CHARACTER" }
open (1, blank=1d1) ! { dg-error "BLANK tag at ... must be of type CHARACTER" }
open (1, blank=.false.) ! { dg-error "BLANK tag at ... must be of type CHARACTER" }
open (1, blank='') ! { dg-error "BLANK specifier in OPEN statement at ... has invalid value" }
open (1, blank='no') ! { dg-error "BLANK specifier in OPEN statement at ... has invalid value" }
open (1, blank=null()) ! { dg-error "BLANK tag at ... must be of type CHARACTER" }
open (1, blank=(1)) ! { dg-error "BLANK tag at ... must be of type CHARACTER" }
open (1, blank=(1., 0.)) ! { dg-error "BLANK tag at ... must be of type CHARACTER" }
open (1, blank=[1]) ! { dg-error "BLANK tag at ... must be of type CHARACTER" }
open (1, blank=['']) ! { dg-error "BLANK tag at ... must be scalar" }

open (1, delim=1) ! { dg-error "DELIM tag at ... must be of type CHARACTER" }
open (1, delim=1e1) ! { dg-error "DELIM tag at ... must be of type CHARACTER" }
open (1, delim=1d1) ! { dg-error "DELIM tag at ... must be of type CHARACTER" }
open (1, delim=.false.) ! { dg-error "DELIM tag at ... must be of type CHARACTER" }
open (1, delim='') ! { dg-error "DELIM specifier in OPEN statement at ... has invalid value" }
open (1, delim='no') ! { dg-error "DELIM specifier in OPEN statement at ... has invalid value" }
open (1, delim=null()) ! { dg-error "DELIM tag at ... must be of type CHARACTER" }
open (1, delim=(1)) ! { dg-error "DELIM tag at ... must be of type CHARACTER" }
open (1, delim=(1., 0.)) ! { dg-error "DELIM tag at ... must be of type CHARACTER" }
open (1, delim=[1]) ! { dg-error "DELIM tag at ... must be of type CHARACTER" }
open (1, delim=['']) ! { dg-error "DELIM tag at ... must be scalar" }

open (1, decimal=1) ! { dg-error "DECIMAL tag at ... must be of type CHARACTER" }
open (1, decimal=1e1) ! { dg-error "DECIMAL tag at ... must be of type CHARACTER" }
open (1, decimal=1d1) ! { dg-error "DECIMAL tag at ... must be of type CHARACTER" }
open (1, decimal=.false.) ! { dg-error "DECIMAL tag at ... must be of type CHARACTER" }
open (1, decimal='') ! { dg-error "DECIMAL specifier in OPEN statement at ... has invalid value" }
open (1, decimal='no') ! { dg-error "DECIMAL specifier in OPEN statement at ... has invalid value" }
open (1, decimal=null()) ! { dg-error "DECIMAL tag at ... must be of type CHARACTER" }
open (1, decimal=(1)) ! { dg-error "DECIMAL tag at ... must be of type CHARACTER" }
open (1, decimal=(1., 0.)) ! { dg-error "DECIMAL tag at ... must be of type CHARACTER" }
open (1, decimal=[1]) ! { dg-error "DECIMAL tag at ... must be of type CHARACTER" }
open (1, decimal=['']) ! { dg-error "DECIMAL tag at ... must be scalar" }

open (1, encoding=1) ! { dg-error "ENCODING tag at ... must be of type CHARACTER" }
open (1, encoding=1e1) ! { dg-error "ENCODING tag at ... must be of type CHARACTER" }
open (1, encoding=1d1) ! { dg-error "ENCODING tag at ... must be of type CHARACTER" }
open (1, encoding=.false.) ! { dg-error "ENCODING tag at ... must be of type CHARACTER" }
open (1, encoding='') ! { dg-error "ENCODING specifier in OPEN statement at ... has invalid value" }
open (1, encoding='no') ! { dg-error "ENCODING specifier in OPEN statement at ... has invalid value" }
open (1, encoding=null()) ! { dg-error "ENCODING tag at ... must be of type CHARACTER" }
open (1, encoding=(1)) ! { dg-error "ENCODING tag at ... must be of type CHARACTER" }
open (1, encoding=(1., 0.)) ! { dg-error "ENCODING tag at ... must be of type CHARACTER" }
open (1, encoding=[1]) ! { dg-error "ENCODING tag at ... must be of type CHARACTER" }
open (1, encoding=['']) ! { dg-error "ENCODING tag at ... must be scalar" }

open (1, form=1) ! { dg-error "FORM tag at ... must be of type CHARACTER" }
open (1, form=1e1) ! { dg-error "FORM tag at ... must be of type CHARACTER" }
open (1, form=1d1) ! { dg-error "FORM tag at ... must be of type CHARACTER" }
open (1, form=.false.) ! { dg-error "FORM tag at ... must be of type CHARACTER" }
open (1, form='') ! { dg-error "FORM specifier in OPEN statement at ... has invalid value" }
open (1, form='no') ! { dg-error "FORM specifier in OPEN statement at ... has invalid value" }
open (1, form=null()) ! { dg-error "FORM tag at ... must be of type CHARACTER" }
open (1, form=(1)) ! { dg-error "FORM tag at ... must be of type CHARACTER" }
open (1, form=(1., 0.)) ! { dg-error "FORM tag at ... must be of type CHARACTER" }
open (1, form=[1]) ! { dg-error "FORM tag at ... must be of type CHARACTER" }
open (1, form=['']) ! { dg-error "FORM tag at ... must be scalar" }

open (1, iomsg=1) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
open (1, iomsg=1e1) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
open (1, iomsg=1d1) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
open (1, iomsg=.false.) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
open (1, iomsg='') ! { dg-error "Non-variable expression" }
open (1, iomsg='no') ! { dg-error "Non-variable expression" }
open (1, iomsg=null()) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
open (1, iomsg=(1)) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
open (1, iomsg=(1., 0.)) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
open (1, iomsg=[1]) ! { dg-error "IOMSG tag at ... must be of type CHARACTER" }
open (1, iomsg=['']) ! { dg-error "IOMSG tag at ... must be scalar" }

open (1, pad=1) ! { dg-error "PAD tag at ... must be of type CHARACTER" }
open (1, pad=1e1) ! { dg-error "PAD tag at ... must be of type CHARACTER" }
open (1, pad=1d1) ! { dg-error "PAD tag at ... must be of type CHARACTER" }
open (1, pad=.false.) ! { dg-error "PAD tag at ... must be of type CHARACTER" }
open (1, pad='') ! { dg-error "PAD specifier in OPEN statement at ... has invalid value" }
open (1, pad='no')
open (1, pad=null()) ! { dg-error "PAD tag at ... must be of type CHARACTER" }
open (1, pad=(1)) ! { dg-error "PAD tag at ... must be of type CHARACTER" }
open (1, pad=(1., 0.)) ! { dg-error "PAD tag at ... must be of type CHARACTER" }
open (1, pad=[1]) ! { dg-error "PAD tag at ... must be of type CHARACTER" }
open (1, pad=['']) ! { dg-error "PAD tag at ... must be scalar" }

open (1, position=1) ! { dg-error "POSITION tag at ... must be of type CHARACTER" }
open (1, position=1e1) ! { dg-error "POSITION tag at ... must be of type CHARACTER" }
open (1, position=1d1) ! { dg-error "POSITION tag at ... must be of type CHARACTER" }
open (1, position=.false.) ! { dg-error "POSITION tag at ... must be of type CHARACTER" }
open (1, position='') ! { dg-error "POSITION specifier in OPEN statement at ... has invalid value" }
open (1, position='no') ! { dg-error "POSITION specifier in OPEN statement at ... has invalid value" }
open (1, position=null()) ! { dg-error "POSITION tag at ... must be of type CHARACTER" }
open (1, position=(1)) ! { dg-error "POSITION tag at ... must be of type CHARACTER" }
open (1, position=(1., 0.)) ! { dg-error "POSITION tag at ... must be of type CHARACTER" }
open (1, position=[1]) ! { dg-error "POSITION tag at ... must be of type CHARACTER" }
open (1, position=['']) ! { dg-error "POSITION tag at ... must be scalar" }

open (1, round=1) ! { dg-error "ROUND tag at ... must be of type CHARACTER" }
open (1, round=1e1) ! { dg-error "ROUND tag at ... must be of type CHARACTER" }
open (1, round=1d1) ! { dg-error "ROUND tag at ... must be of type CHARACTER" }
open (1, round=.false.) ! { dg-error "ROUND tag at ... must be of type CHARACTER" }
open (1, round='') ! { dg-error "ROUND specifier in OPEN statement at ... has invalid value" }
open (1, round='no') ! { dg-error "ROUND specifier in OPEN statement at ... has invalid value" }
open (1, round=null()) ! { dg-error "ROUND tag at ... must be of type CHARACTER" }
open (1, round=(1)) ! { dg-error "ROUND tag at ... must be of type CHARACTER" }
open (1, round=(1., 0.)) ! { dg-error "ROUND tag at ... must be of type CHARACTER" }
open (1, round=[1]) ! { dg-error "ROUND tag at ... must be of type CHARACTER" }
open (1, round=['']) ! { dg-error "ROUND tag at ... must be scalar" }

open (1, sign=1) ! { dg-error "SIGN tag at ... must be of type CHARACTER" }
open (1, sign=1e1) ! { dg-error "SIGN tag at ... must be of type CHARACTER" }
open (1, sign=1d1) ! { dg-error "SIGN tag at ... must be of type CHARACTER" }
open (1, sign=.false.) ! { dg-error "SIGN tag at ... must be of type CHARACTER" }
open (1, sign='') ! { dg-error "SIGN specifier in OPEN statement at ... has invalid value" }
open (1, sign='no') ! { dg-error "SIGN specifier in OPEN statement at ... has invalid value" }
open (1, sign=null()) ! { dg-error "SIGN tag at ... must be of type CHARACTER" }
open (1, sign=(1)) ! { dg-error "SIGN tag at ... must be of type CHARACTER" }
open (1, sign=(1., 0.)) ! { dg-error "SIGN tag at ... must be of type CHARACTER" }
open (1, sign=[1]) ! { dg-error "SIGN tag at ... must be of type CHARACTER" }
open (1, sign=['']) ! { dg-error "SIGN tag at ... must be scalar" }

open (1, status=1) ! { dg-error "STATUS tag at ... must be of type CHARACTER" }
open (1, status=1e1) ! { dg-error "STATUS tag at ... must be of type CHARACTER" }
open (1, status=1d1) ! { dg-error "STATUS tag at ... must be of type CHARACTER" }
open (1, status=.false.) ! { dg-error "STATUS tag at ... must be of type CHARACTER" }
open (1, status='') ! { dg-error "STATUS specifier in OPEN statement at ... has invalid value" }
open (1, status='no') ! { dg-error "STATUS specifier in OPEN statement at ... has invalid value" }
open (1, status=null()) ! { dg-error "STATUS tag at ... must be of type CHARACTER" }
open (1, status=(1)) ! { dg-error "STATUS tag at ... must be of type CHARACTER" }
open (1, status=(1., 0.)) ! { dg-error "STATUS tag at ... must be of type CHARACTER" }
open (1, status=[1]) ! { dg-error "STATUS tag at ... must be of type CHARACTER" }
open (1, status=['']) ! { dg-error "STATUS tag at ... must be scalar" }


end
