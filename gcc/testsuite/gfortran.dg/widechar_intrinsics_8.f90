! { dg-do run }
! { dg-options "-fbackslash" }

  logical, parameter :: bigendian = transfer ((/1_1,0_1,0_1,0_1/), 0_4) /= 1

  character(kind=1,len=3) :: s1, t1, u1
  character(kind=4,len=3) :: s4, t4, u4

  ! Test MERGE intrinsic

  call check_merge1 ("foo", "gee", .true., .false.)
  call check_merge4 (4_"foo", 4_"gee", .true., .false.)

  if (merge ("foo", "gee", .true.) /= "foo") call abort
  if (merge ("foo", "gee", .false.) /= "gee") call abort
  if (merge (4_"foo", 4_"gee", .true.) /= 4_"foo") call abort
  if (merge (4_"foo", 4_"gee", .false.) /= 4_"gee") call abort

  ! Test TRANSFER intrinsic

  if (bigendian) then
    if (transfer (4_"x", "    ") /= "\0\0\0x") call abort
  else
    if (transfer (4_"x", "    ") /= "x\0\0\0") call abort
  endif
  if (transfer (4_"\U44444444", "    ") /= "\x44\x44\x44\x44") call abort
  if (transfer (4_"\U3FE91B5A", 0_4) /= int(z'3FE91B5A', 4)) call abort

  call check_transfer_i (4_"\U3FE91B5A", [int(z'3FE91B5A', 4)])
  call check_transfer_i (4_"\u1B5A", [int(z'1B5A', 4)])

contains

  subroutine check_merge1 (s1, t1, t, f)
    character(kind=1,len=*) :: s1, t1
    logical :: t, f

    if (merge (s1, t1, .true.) /= s1) call abort
    if (merge (s1, t1, .false.) /= t1) call abort
    if (len (merge (s1, t1, .true.)) /= len (s1)) call abort
    if (len (merge (s1, t1, .false.)) /= len (t1)) call abort
    if (len_trim (merge (s1, t1, .true.)) /= len_trim (s1)) call abort
    if (len_trim (merge (s1, t1, .false.)) /= len_trim (t1)) call abort

    if (merge (s1, t1, t) /= s1) call abort
    if (merge (s1, t1, f) /= t1) call abort
    if (len (merge (s1, t1, t)) /= len (s1)) call abort
    if (len (merge (s1, t1, f)) /= len (t1)) call abort
    if (len_trim (merge (s1, t1, t)) /= len_trim (s1)) call abort
    if (len_trim (merge (s1, t1, f)) /= len_trim (t1)) call abort

  end subroutine check_merge1

  subroutine check_merge4 (s4, t4, t, f)
    character(kind=4,len=*) :: s4, t4
    logical :: t, f

    if (merge (s4, t4, .true.) /= s4) call abort
    if (merge (s4, t4, .false.) /= t4) call abort
    if (len (merge (s4, t4, .true.)) /= len (s4)) call abort
    if (len (merge (s4, t4, .false.)) /= len (t4)) call abort
    if (len_trim (merge (s4, t4, .true.)) /= len_trim (s4)) call abort
    if (len_trim (merge (s4, t4, .false.)) /= len_trim (t4)) call abort

    if (merge (s4, t4, t) /= s4) call abort
    if (merge (s4, t4, f) /= t4) call abort
    if (len (merge (s4, t4, t)) /= len (s4)) call abort
    if (len (merge (s4, t4, f)) /= len (t4)) call abort
    if (len_trim (merge (s4, t4, t)) /= len_trim (s4)) call abort
    if (len_trim (merge (s4, t4, f)) /= len_trim (t4)) call abort

  end subroutine check_merge4

  subroutine check_transfer_i (s, i)
    character(kind=4,len=*) :: s
    integer(kind=4), dimension(len(s)) :: i

    if (transfer (s, 0_4) /= ichar (s(1:1))) call abort
    if (transfer (s, 0_4) /= i(1)) call abort
    if (any (transfer (s, [0_4]) /= i)) call abort
    if (any (transfer (s, 0_4, len(s)) /= i)) call abort

  end subroutine check_transfer_i

end
