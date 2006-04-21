! { dg-do run }
! Tests the fix for PR27113, in which character structure
! components would produce the TODO compilation error "complex
! character array constructors".
!
! Test based on part of tonto-2.2;
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
  type BASIS_TYPE
    character(len=8) :: label
  end type

  type(BASIS_TYPE), dimension(:), pointer :: ptr
  character(8), dimension(2) :: carray

  allocate (ptr(2))
  ptr(1)%label = "Label 1"
  ptr(2)%label = "Label 2"

! This is the original bug
  call read_library_data_((/ptr%label/))

  carray(1) = "Label 3"
  carray(2) = "Label 4"

! Mix a character array with the character component of a derived type pointer array.
  call read_library_data_((/carray, ptr%label/))

! Finally, add a constant (character(8)).
  call read_library_data_((/carray, ptr%label, "Label 5 "/))

contains

  subroutine read_library_data_ (chr)
    character(*), dimension(:) :: chr
    character(len = len(chr)) :: tmp
    if (size(chr,1) == 2) then
      if (any (chr .ne. (/"Label 1", "Label 2"/))) call abort ()
    elseif (size(chr,1) == 4) then
      if (any (chr .ne. (/"Label 3", "Label 4","Label 1", "Label 2"/))) call abort ()
    elseif (size(chr,1) == 5) then
      if (any (chr .ne. (/"Label 3", "Label 4","Label 1", "Label 2", "Label 5"/))) &
          call abort ()
    end if
  end subroutine read_library_data_

end
