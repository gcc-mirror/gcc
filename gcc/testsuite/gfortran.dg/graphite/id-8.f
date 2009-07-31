      subroutine foo(mxgtot,mxsh)
      logical b
      dimension ex(mxgtot),cs(mxgtot)
         do 500 jg = k1,ig
            u = ex(ig)+ex(jg)
            z = u*sqrt(u)
            x = cs(ig)*cs(jg)/z
            if (ig .eq. jg) go to 480
               x = x+x
  480       continue
            y = y+x
  500    continue
      if(y.gt.t) z=1/sqrt(y)
      if (b) then
         write(9) z
      endif
      end
