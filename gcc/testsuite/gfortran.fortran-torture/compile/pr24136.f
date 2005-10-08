      subroutine  electra(ro,t,ye,ee,pe,se
     a                   ,eer,eet,per,pet,ser,set,keyps)
      implicit real*8 (a-h,o-z)
      common  /nunu/  nu,dnudr,dnudb,eta,detadnu,nup
      data facen,facpr,facs,rg /2.037300d+24,1.358200d+24,1.686304d-10
     1,8.314339d+07/
      data a1,a2,a3,a4 /2.059815d-03,-7.027778d-03
     1,4.219747d-02,-1.132427d+00/
      beta=facs*t
      b32=b12*beta
      u=(f62/f52)**2
      dudnu=2.0d0*u*(df62/f62-df52/f52)
      x=beta*u
      f=1.0d0+x*(2.5d0+x*(2.0d0+0.5d0*x))
      df=2.5d0+x*(4.0d0+1.5d0*x)
      dfdb=u*df
      fi32=f32+(f-1.0d0)*f52/u
      dfidnu=dfidu*dudnu+df32+(f-1.0d0)*df52/u
      dfidb=dfdb*f52/u
      dfidbet=dfidb+dfidnu*dnudb
      gs=sqrt(g)
      dg=0.75d0*gs
      dgdb=u*dg
      dgdu=beta*dg
      gi32=f32+(g-1.0d0)*f52/u
      dgidu=(u*dgdu-g+1.0d0)*f52/us
      dgidnu=dgidu*dudnu+df32+(g-1.0d0)*df52/u
      dgidb=dgdb*f52/u
      dgidbet=dgidb+dgidnu*dnudb
      dgidroe=dgidnu*dnudr
      em=facen*b52*fi32
      demdbet=facen*b32*(2.5d0*fi32+beta*dfidbet)
      dpmdbet=facpr*b32*(2.5d0*gi32+beta*dgidbet)
      demdroe=facen*b52*dfidroe
      dpmdroe=facpr*b52*dgidroe
      call  divine(nup,fp12,dfp12,s12)
      s42=2.0d0
      call  divine(nup,fp42,dfp42,s42)
      eer=(ye*(demdroe+depdroe)-(em+ep)/ro)/ro
      eet=facs*(demdbet+depdbet)/ro
      per=ye*(dpmdroe+dppdroe)
      pet=facs*(dpmdbet+dppdbet)
      end
