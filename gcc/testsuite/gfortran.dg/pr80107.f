! { dg-do compile { target { powerpc*-*-* } } }
! { dg-options "-O0 -mpower9-dform-vector -mno-gen-cell-microcode" }

      integer(kind=2) j, j2, ja
      call c_c(CMPLX(j),(1.,0.),'CMPLX(integer(2))')
      end
