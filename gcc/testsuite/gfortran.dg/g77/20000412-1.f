c { dg-do compile }
      subroutine aap(k)
      equivalence (i,r)
      i = k
      print*,r
      end
