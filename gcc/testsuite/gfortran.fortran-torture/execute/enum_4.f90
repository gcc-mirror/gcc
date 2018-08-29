! Program to test the default initialisation of enumerators inside different program unit

module mod
  implicit none
  enum, bind (c)
    enumerator :: red , yellow, blue
    enumerator :: green
  end enum
end module mod

program main
  use mod
  implicit none

  if (red /= 0 ) STOP 1
  if (yellow /= 1) STOP 2
  if (blue /= 2) STOP 3
  if (green /= 3) STOP 4
end program main
