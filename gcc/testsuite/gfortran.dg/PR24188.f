C PR target/24188
C { dg-do compile }
C { dg-options "-O2" }
C { dg-options "-O2 -mcmodel=medium" { target { x86_64-*-* && lp64 } } }
C { dg-options "-O2 -mcmodel=medium" { target { i?86-*-* && lp64 } } }
      WRITE(6,*) ''
      END
