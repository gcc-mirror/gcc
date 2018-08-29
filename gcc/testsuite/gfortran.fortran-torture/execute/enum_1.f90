! Program to test the default initialisation of enumerators 

program main
  implicit none

  enum, bind (c)
    enumerator :: red , yellow, blue
    enumerator :: green
  end enum

  enum, bind (c)
    enumerator :: a , b , c = 10
    enumerator :: d
  end enum


  if (red /= 0 ) STOP 1
  if (yellow /= 1) STOP 2
  if (blue /= 2) STOP 3
  if (green /= 3) STOP 4

  if (a /= 0 ) STOP 5
  if (b /= 1) STOP 6
  if (c /= 10) STOP 7
  if (d /= 11) STOP 8

  
end program main
