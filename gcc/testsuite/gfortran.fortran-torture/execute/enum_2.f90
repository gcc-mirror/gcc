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


  if (red /= 4 ) call abort
  if (yellow /= (red + 1)) call abort
  if (blue /= (yellow + 1)) call abort
  if (green /= (blue + 1)) call abort
  

  if (sun /= -10 ) call abort
  if (mon /= (sun + 1)) call abort
  if (tue /= (mon + 1)) call abort
  if (wed /= 10) call abort
  if (sat /= (wed+1)) call abort

end program main
