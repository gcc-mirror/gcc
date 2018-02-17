! { dg-do run }
! { dg-options "-fbackslash" }

module kinds
  implicit none
  integer, parameter :: one = 1, four = 4
end module kinds

module inner
  use kinds
  implicit none
  character(kind=one,len=*), parameter :: inner1 = "abcdefg \xEF kl"
  character(kind=four,len=*), parameter :: &
        inner4 = 4_"\u9317x \U001298cef   dea\u10De"
end module inner

module middle
  use inner
  implicit none
  character(kind=one,len=len(inner1)), dimension(2,2), parameter :: middle1 &
    = reshape ([ character(kind=one,len=len(inner1)) :: inner1, ""], &
               [ 2, 2 ], &
               [ character(kind=one,len=len(inner1)) :: "foo", "ba " ])
  character(kind=four,len=len(inner4)), dimension(2,2), parameter :: middle4 &
    = reshape ([ character(kind=four,len=len(inner4)) :: inner4, 4_""], &
               [ 2, 2 ], &
               [ character(kind=four,len=len(inner4)) :: 4_"foo", 4_"ba " ])
end module middle

module outer
  use middle
  implicit none
  character(kind=one,len=*), parameter :: my1(2) = middle1(1,:)
  character(kind=four,len=*), parameter :: my4(2) = middle4(1,:)
end module outer

program test_modules
  use outer, outer1 => my1, outer4 => my4
  implicit none

  if (len (inner1) /= len(inner4)) STOP 1
  if (len (inner1) /= len_trim(inner1)) STOP 2
  if (len (inner4) /= len_trim(inner4)) STOP 3

  if (len(middle1) /= len(inner1)) STOP 4
  if (len(outer1) /= len(inner1)) STOP 5
  if (len(middle4) /= len(inner4)) STOP 6
  if (len(outer4) /= len(inner4)) STOP 7

  if (any (len_trim (middle1) /= reshape([len(middle1), 0, 3, 2], [2,2]))) &
    STOP 8
  if (any (len_trim (middle4) /= reshape([len(middle4), 0, 3, 2], [2,2]))) &
    STOP 9
  if (any (len_trim (outer1) /= [len(outer1), 3])) STOP 10
  if (any (len_trim (outer4) /= [len(outer4), 3])) STOP 11

end program test_modules
