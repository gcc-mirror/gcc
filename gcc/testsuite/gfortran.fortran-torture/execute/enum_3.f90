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


 if (red /= 0 ) STOP 1
 if (yellow /= 255) STOP 2
 if (blue /= 256) STOP 3
  
 if (r /= 0 ) STOP 4
 if (y /= 32767) STOP 5
 if (b /= 32768) STOP 6

 if (kind (red) /= 4) STOP 7
 if (kind (yellow) /= 4) STOP 8
 if (kind (blue) /= 4) STOP 9

 if (kind(r) /= 4 ) STOP 10
 if (kind(y) /= 4) STOP 11
 if (kind(b) /= 4) STOP 12

 if (aa /= 0 ) STOP 13
 if (bb /= 65535) STOP 14
 if (cc /= 65536) STOP 15

 if (kind (aa) /= 4 ) STOP 16
 if (kind (bb) /= 4) STOP 17
 if (kind (cc) /= 4) STOP 18


 if (m /= 0 ) STOP 19
 if (n /= 2147483645) STOP 20
 if (o /= 2147483646) STOP 21

 if (kind (m) /= 4 ) STOP 22
 if (kind (n) /= 4) STOP 23
 if (kind (o) /= 4) STOP 24

end program main
