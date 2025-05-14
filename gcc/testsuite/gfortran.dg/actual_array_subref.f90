! { dg-do run }
! { dg-additional-options "-O2 -fcheck=bounds" }
!
! PR fortran/119986
!
! Check passing of inquiry references of complex arrays and substring
! references of character arrays when these are components of derived types.
!
! Extended version of report by Neil Carlson.

program main
  implicit none
  integer :: j

  complex, parameter  :: z0(*) = [(cmplx(j,-j),j=1,4)]
  type :: cx
     real :: re
     real :: im
  end type cx
  type(cx), parameter :: c0(*) = [(cx   (j,-j),j=1,4)]

  type :: my_type
     complex  :: z(4) = z0
     type(cx) :: c(4) = c0
  end type my_type
  type(my_type) :: x

  character(*), parameter :: s0(*) = ["abcd","efgh","ijkl","mnop"]
  character(*), parameter :: expect(*) = s0(:)(2:3)
  character(len(s0))      :: s1(4) = s0

  type :: str1
     character(len(s0))   :: s(4)  = s0
  end type str1
  type(str1) :: string1

  type :: str2
     character(:), allocatable :: s(:)
  end type str2
  type(str2) :: string2

  integer :: stopcode = 0

  if (len(expect) /= 2)    stop 1
  if (expect(4)   /= "no") stop 2
  if (any(c0 %re  /= [ 1, 2, 3, 4])) stop 3
  if (any(c0 %im  /= [-1,-2,-3,-4])) stop 4

  stopcode = 10
  call fubar ( x%z %re, x%z %im)
  call fubar ( x%c %re, x%c %im)

  stopcode = 20
  call fubar ((x%z %re), (x%z %im))
  call fubar ((x%c %re), (x%c %im))

  stopcode = 30
  call fubar ([x%z %re], [x%z %im])
  call fubar ([x%c %re], [x%c %im])

  stopcode = 50
  call chk ( s0(:)(2:3) )
  call chk ((s0(:)(2:3)))
  call chk ([s0(:)(2:3)])

  stopcode = 60
  call chk ( s1(:)(2:3) )
  call chk ((s1(:)(2:3)))
  call chk ([s1(:)(2:3)])

  stopcode = 70
  call chk ( string1%s(:)(2:3) )
  call chk ((string1%s(:)(2:3)))
  call chk ([string1%s(:)(2:3)])

  string2% s = s0
  if (len(string2%s) /= 4) stop 99
  stopcode = 80
  call chk ( string2%s(:)(2:3) )
  call chk ((string2%s(:)(2:3)))
  call chk ([string2%s(:)(2:3)])
  deallocate (string2% s)

contains

  subroutine fubar(u, v)
    real, intent(in) :: u(:), v(:)
    if (any (u /= z0%re)) stop stopcode + 1
    if (any (v /= z0%im)) stop stopcode + 2
    if (any (u /= c0%re)) stop stopcode + 3
    if (any (v /= c0%im)) stop stopcode + 4
    stopcode = stopcode + 4
  end subroutine

  subroutine chk (s)
    character(*), intent(in) :: s(:)
    if (size(s) /= 4)      stop stopcode + 1
    if (len (s) /= 2)      stop stopcode + 2
    if (any (s /= expect)) stop stopcode + 3
    stopcode = stopcode + 3
  end subroutine chk

end program
