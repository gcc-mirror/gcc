! { dg-do run }
! { dg-options "-O -fdump-tree-original" }
! PR 47065 - replace trim with substring expressions even with references.
program main
  implicit none
  type t
     character(len=2) :: x
  end type t
  type(t) :: a
  character(len=3) :: b
  character(len=10) :: line
  a%x = 'a'
  write(unit=line,fmt='(A,A)') trim(a%x),"X"
  if (line /= 'aX        ') call abort
  b = 'ab'
  write (unit=line,fmt='(A,A)') trim(b),"Y"
  if (line /= 'abY       ') call abort
end program main
! { dg-final { scan-tree-dump-times "string_len_trim" 2 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
