      subroutine detune(iv,ekk,ep,beta,dtu,dtup,dfac)
      implicit real*8 (a-h,o-z)
      parameter(npart=64,nmac=1)
      parameter(nele=700,nblo=300,nper=16,
     &nelb=100,nblz=20000,nzfz=300000,mmul=11)
      parameter(nran=280000,ncom=100,mran=500,mpa=6,nrco=5,nema=15)
      parameter(mcor=10)
      parameter(npos=20000,nlya=10000,ninv=1000,nplo=20000)
      parameter(nmon1=600,ncor1=600)
      parameter(pieni=1d-17)
      parameter(zero=0.0d0,half=0.5d0,one=1.0d0)
      parameter(two=2.0d0,three=3.0d0,four=4.0d0)
      dimension dfac(10),dtu(2,5),ep(2),beta(2),dtup(2,5,0:4,0:4)
      save
      pi=four*atan(one)
      iv2=2*iv
      iv3=iv+1
      vtu1=-ekk*(half**iv2)*dfac(iv2)/pi
      dtu1=zero
      dtu2=zero
      do 10 iv4=1,iv3
        iv5=iv4-1
        iv6=iv-iv5
        vor=one
        if(mod(iv6,2).ne.0) vor=-one
        vtu2=vor/(dfac(iv5+1)**2)/(dfac(iv6+1)**2)*(beta(1)**iv5)* (beta
     +  (2)**iv6)
        if(iv5.ne.0) then
          dtu1=dtu1+vtu2*iv5*(ep(1)**(iv5-1))*(ep(2)**iv6)
          dtup(1,iv,iv5-1,iv6)=dtup(1,iv,iv5-1,iv6)+vtu2*iv5*vtu1
        endif
        if(iv6.ne.0) then
          dtu2=dtu2+vtu2*iv6*(ep(1)**iv5)*(ep(2)**(iv6-1))
          dtup(2,iv,iv5,iv6-1)=dtup(2,iv,iv5,iv6-1)+vtu2*iv6*vtu1
        endif
   10 continue
      dtu(1,iv)=dtu(1,iv)+vtu1*dtu1
      dtu(2,iv)=dtu(2,iv)+vtu1*dtu2
      return
      end
