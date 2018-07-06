! { dg-lto-do link }
! { dg-lto-options {{ -O2 -flto -g }} }
! { dg-extra-ld-options "-r -nostdlib -flinker-output=nolto-rel" }
      common/main1/ eps(2)
      call dalie6s(iqmod6,1,wx,cor6d)
      end
