! Program to test the incremental assignment of enumerators

program main
  implicit none

  enum, bind (c)
    enumerator :: red = 4 , yellow, blue
    enumerator  green 
  end enum

  enum, bind (c)
    enumerator :: sun = -10 , mon, tue
    enumerator :: wed = 10, sat
  end enum


  if (red /= 4 ) STOP 1
  if (yellow /= (red + 1)) STOP 2
  if (blue /= (yellow + 1)) STOP 3
  if (green /= (blue + 1)) STOP 4
  

  if (sun /= -10 ) STOP 5
  if (mon /= (sun + 1)) STOP 6
  if (tue /= (mon + 1)) STOP 7
  if (wed /= 10) STOP 8
  if (sat /= (wed+1)) STOP 9

end program main
