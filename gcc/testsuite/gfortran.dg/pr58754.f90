! { dg-do compile }
!
! Tests the fix for PR58754
!
  type :: char_type
    character, allocatable :: chr (:)
  end type
  character, allocatable :: c(:)
  type(char_type) :: d
  character :: t(1) = ["w"]

  allocate (c (1), source = t)
  if (any (c .ne. t)) call abort
  c = ["a"]
  if (any (c .ne. ["a"])) call abort
  deallocate (c)

! Check allocatable character components, whilst we are about it.
  allocate (d%chr (2), source = [t, char (ichar (t) + 1)])
  if (any (d%chr .ne. ["w", "x"])) call abort
  d%chr = ["a","b","c","d"]
  if (any (d%chr .ne. ["a","b","c","d"])) call abort
  deallocate (d%chr)
end
