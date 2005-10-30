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


  if (red /= 0 ) call abort
  if (yellow /= 1) call abort
  if (blue /= 2) call abort
  if (green /= 3) call abort

  if (a /= 0 ) call abort
  if (b /= 1) call abort
  if (c /= 10) call abort
  if (d /= 11) call abort

  
end program main
