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

  if (red /= 0 ) call abort
  if (yellow /= 1) call abort
  if (blue /= 2) call abort
  if (green /= 3) call abort
end program main
