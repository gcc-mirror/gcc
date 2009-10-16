! { dg-lto-do link }
! We expect some warnings about mismatched symbol types
! { dg-extra-ld-options "-w" }

      subroutine dalie6s(iqmod6,nz,wx,cor6d)
      common/dascr/iscrda(100),rscrri(100),iscrri(100),idao
      call daall(iscrda,100,'$$IS      ',no,nv)
      end
