! Program to test the initialisation range of enumerators 
! and kind values check

program main
  implicit none

  enum, bind (c)
    enumerator :: red , yellow =255 , blue 
  end enum

  enum, bind (c)
    enumerator :: r , y = 32767, b
  end enum

  enum, bind (c)
    enumerator :: aa , bb = 65535, cc
  end enum

  enum, bind (c)
    enumerator :: m , n = 2147483645, o
  end enum


 if (red /= 0 ) call abort
 if (yellow /= 255) call abort
 if (blue /= 256) call abort
  
 if (r /= 0 ) call abort
 if (y /= 32767) call abort
 if (b /= 32768) call abort

 if (kind (red) /= 4) call abort
 if (kind (yellow) /= 4) call abort
 if (kind (blue) /= 4) call abort

 if (kind(r) /= 4 ) call abort
 if (kind(y) /= 4) call abort
 if (kind(b) /= 4) call abort

 if (aa /= 0 ) call abort
 if (bb /= 65535) call abort
 if (cc /= 65536) call abort

 if (kind (aa) /= 4 ) call abort
 if (kind (bb) /= 4) call abort
 if (kind (cc) /= 4) call abort


 if (m /= 0 ) call abort
 if (n /= 2147483645) call abort
 if (o /= 2147483646) call abort

 if (kind (m) /= 4 ) call abort
 if (kind (n) /= 4) call abort
 if (kind (o) /= 4) call abort

end program main
