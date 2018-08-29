! { dg-do run }
! { dg-options "-fbackslash" }

  logical, parameter :: bigendian = transfer ((/1_1,0_1,0_1,0_1/), 0_4) /= 1

  character(kind=1,len=3) :: s1, t1, u1
  character(kind=4,len=3) :: s4, t4, u4

  ! Test MERGE intrinsic

  call check_merge1 ("foo", "gee", .true., .false.)
  call check_merge4 (4_"foo", 4_"gee", .true., .false.)

  if (merge ("foo", "gee", .true.) /= "foo") STOP 1
  if (merge ("foo", "gee", .false.) /= "gee") STOP 2
  if (merge (4_"foo", 4_"gee", .true.) /= 4_"foo") STOP 3
  if (merge (4_"foo", 4_"gee", .false.) /= 4_"gee") STOP 4

  ! Test TRANSFER intrinsic

  if (bigendian) then
    if (transfer (4_"x", "    ") /= "\0\0\0x") STOP 5
  else
    if (transfer (4_"x", "    ") /= "x\0\0\0") STOP 6
  endif
  if (transfer (4_"\U44444444", "    ") /= "\x44\x44\x44\x44") STOP 7
  if (transfer (4_"\U3FE91B5A", 0_4) /= int(z'3FE91B5A', 4)) STOP 8

  call check_transfer_i (4_"\U3FE91B5A", [int(z'3FE91B5A', 4)])
  call check_transfer_i (4_"\u1B5A", [int(z'1B5A', 4)])

contains

  subroutine check_merge1 (s1, t1, t, f)
    character(kind=1,len=*) :: s1, t1
    logical :: t, f

    if (merge (s1, t1, .true.) /= s1) STOP 9
    if (merge (s1, t1, .false.) /= t1) STOP 10
    if (len (merge (s1, t1, .true.)) /= len (s1)) STOP 11
    if (len (merge (s1, t1, .false.)) /= len (t1)) STOP 12
    if (len_trim (merge (s1, t1, .true.)) /= len_trim (s1)) STOP 13
    if (len_trim (merge (s1, t1, .false.)) /= len_trim (t1)) STOP 14

    if (merge (s1, t1, t) /= s1) STOP 15
    if (merge (s1, t1, f) /= t1) STOP 16
    if (len (merge (s1, t1, t)) /= len (s1)) STOP 17
    if (len (merge (s1, t1, f)) /= len (t1)) STOP 18
    if (len_trim (merge (s1, t1, t)) /= len_trim (s1)) STOP 19
    if (len_trim (merge (s1, t1, f)) /= len_trim (t1)) STOP 20

  end subroutine check_merge1

  subroutine check_merge4 (s4, t4, t, f)
    character(kind=4,len=*) :: s4, t4
    logical :: t, f

    if (merge (s4, t4, .true.) /= s4) STOP 21
    if (merge (s4, t4, .false.) /= t4) STOP 22
    if (len (merge (s4, t4, .true.)) /= len (s4)) STOP 23
    if (len (merge (s4, t4, .false.)) /= len (t4)) STOP 24
    if (len_trim (merge (s4, t4, .true.)) /= len_trim (s4)) STOP 25
    if (len_trim (merge (s4, t4, .false.)) /= len_trim (t4)) STOP 26

    if (merge (s4, t4, t) /= s4) STOP 27
    if (merge (s4, t4, f) /= t4) STOP 28
    if (len (merge (s4, t4, t)) /= len (s4)) STOP 29
    if (len (merge (s4, t4, f)) /= len (t4)) STOP 30
    if (len_trim (merge (s4, t4, t)) /= len_trim (s4)) STOP 31
    if (len_trim (merge (s4, t4, f)) /= len_trim (t4)) STOP 32

  end subroutine check_merge4

  subroutine check_transfer_i (s, i)
    character(kind=4,len=*) :: s
    integer(kind=4), dimension(len(s)) :: i

    if (transfer (s, 0_4) /= ichar (s(1:1))) STOP 33
    if (transfer (s, 0_4) /= i(1)) STOP 34
    if (any (transfer (s, [0_4]) /= i)) STOP 35
    if (any (transfer (s, 0_4, len(s)) /= i)) STOP 36

  end subroutine check_transfer_i

end
