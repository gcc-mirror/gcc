! { dg-do run }
! PR fortran/103115 - character array constructor with >= 4 constant elements
!
! This used to ICE when the first element is deferred-length character
! or could lead to wrong results.

program pr103115
  implicit none
  integer :: i
  character(*), parameter :: expect(*) = [ "1","2","3","4","5" ]
  character(5)            :: abc = "12345"
  character(5), parameter :: def = "12345"
  character(:), dimension(:), allocatable :: list
  character(:), dimension(:), allocatable :: titles
  titles = ["1"]
  titles = [ titles&
            ,"2"&
            ,"3"&
            ,"4"&
            ,"5"&  ! used to ICE
            ]
  if (len (titles) /= 1 .or. size (titles) /= 5) stop 1
  if (any (titles  /= expect))                   stop 2
  titles = ["1"]
  titles = [ titles, (char(48+i),i=2,5) ]
  if (len (titles) /= 1 .or. size (titles) /= 5) stop 3
  if (any (titles  /= expect))                   stop 4
  titles = ["1"]
  titles = [ titles, ("2345"(i:i),i=1,4) ]
  if (len (titles) /= 1 .or. size (titles) /= 5) stop 5
  if (any (titles  /= expect))                   stop 6
  titles = ["1"]
  titles = [ titles, (def(i:i),i=2,5) ]
  if (len (titles) /= 1 .or. size (titles) /= 5) stop 7
  if (any (titles  /= expect))                   stop 8
  list = [ (char(48+i),i=1,5) ]
  titles = [ list(1), (char(48+i),i=2,5) ]
  if (len (titles) /= 1 .or. size (titles) /= 5) stop 9
  if (any (titles  /= expect))                   stop 10
  titles = ["1"]
  titles = [ titles, (abc(i:i),i=2,5) ]
  if (len (titles) /= 1 .or. size (titles) /= 5) stop 11
  if (any (titles  /= expect))                   stop 12

  ! with typespec:
  list = [ (char(48+i),i=1,5) ]
  titles = [ character(2) :: list(1), (char(48+i),i=2,5) ]
  if (len (titles) /= 2 .or. size (titles) /= 5) stop 13
  if (any (titles  /= expect))                   stop 14
  titles = ["1"]
  titles = [ character(2) :: titles, (char(48+i),i=2,5) ]
  if (len (titles) /= 2 .or. size (titles) /= 5) stop 15
  if (any (titles  /= expect))                   stop 16
  titles = ["1"]
  titles = [ character(2) :: titles, (def(i:i),i=2,5) ]
  if (len (titles) /= 2 .or. size (titles) /= 5) stop 17
  if (any (titles  /= expect))                   stop 18
  deallocate (titles, list)
end
