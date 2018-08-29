! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR16805
! Test list directed reads from character substrings
! The IO library was reporting an error rather the end-of-record when it
! got to the end of an internal file record.
program list_read_2
  implicit none
  character*10 a
  data a /'1234567890'/
  integer i
  logical debug
  data debug /.TRUE./
  read(a,*)i
  if (i.ne.1234567890) STOP 1
  read(a(1:1),*)i
  if (i.ne.1) STOP 2
  read(a(2:2),*)i
  if (i.ne.2) STOP 3
  read(a(1:5),*)i
  if (i.ne.12345) STOP 4
  read(a(5:10),*)i
  if (i.ne.567890) STOP 5
  read(a(10:10),*)i
  if (i.ne.0) STOP 6
end
