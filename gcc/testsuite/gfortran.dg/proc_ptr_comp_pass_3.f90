! { dg-do run }
!
! PR 39630: [F03] Procedure Pointer Components with PASS
!
! taken from "Fortran 95/2003 explained" (Metcalf, Reid, Cohen, 2004)

type t
  procedure(obp), pointer, pass(x) :: p
  character(100) :: name
end type

abstract interface
  subroutine obp(w,x)
    import :: t
    integer :: w
    class(t) :: x
  end subroutine
end interface

type(t) :: a
a%p => my_obp_sub
a%name = "doodoo"

call a%p(32)

contains

  subroutine my_obp_sub(w,x)
    integer :: w
    class(t) :: x
    if (x%name/="doodoo") STOP 1
    if (w/=32) STOP 2
  end subroutine

end

