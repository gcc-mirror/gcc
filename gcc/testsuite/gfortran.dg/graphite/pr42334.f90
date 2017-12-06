! { dg-options "-O2 -floop-nest-optimize -ftree-loop-distribution" }

subroutine blockdis(bl1eg,bl2eg)
  implicit real*8 (a-h,o-z)
  parameter(nblo=300)
  common/str /mblo
  common/str2 /mel(nblo)
  dimension h(nblo,2,6),g(nblo,2,6)
  dimension bl1eg(nblo,2,6),bl2eg(nblo,2,6)
  do k=1,mblo
     jm=mel(k)
     do l=1,2
        do m=1,6
           bl1eg(k,l,m)=h(jm,l,m)
           bl2eg(k,l,m)=g(jm,l,m)
        enddo
     enddo
  enddo
  return
end subroutine blockdis
