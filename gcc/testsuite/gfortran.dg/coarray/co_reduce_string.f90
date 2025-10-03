!{ dg-do run }

! Check that co_reduce for strings works.
! This test is motivated by OpenCoarray's co_reduce_string test.

program co_reduce_strings
 
  implicit none

  integer, parameter :: numstrings = 10, strlen = 8, base_len = 4
  character(len=strlen), dimension(numstrings) :: fixarr
  character(len=strlen), dimension(:), allocatable :: allocarr
  character(len=:), allocatable :: defarr(:)
  character(len=strlen) :: expect
  integer :: i

  ! Construct the strings by postfixing foo by a number.
  associate (me => this_image(), np => num_images())
    if (np > 999) error stop "Too many images; increase format string modifiers and sizes!"
    
    allocate(allocarr(numstrings))
    do i = 1, numstrings
      write(fixarr(i), "('foo',I04)") i * me
      write(allocarr(i), "('foo',I04)") i * me
    end do
    ! Collectively reduce the maximum string.
    call co_reduce(fixarr, fixmax)
    call check(fixarr, 1)

    call co_reduce(allocarr, strmax)
    call check(allocarr, 2)
  end associate

  ! Construct the strings by postfixing foo by a number.
  associate (me => this_image(), np => num_images())
    allocate(character(len=base_len + 4)::defarr(numstrings))
    do i = 1, numstrings
      write(defarr(i), "('foo',I04)") i * me
    end do
    call sub_red(defarr)
  end associate
  sync all

contains

  pure function fixmax(lhs, rhs) result(m)
    character(len=strlen), intent(in) :: lhs, rhs
    character(len=strlen) :: m

    if (lhs > rhs) then
      m = lhs
    else
      m = rhs
    end if
  end function

  pure function strmax(lhs, rhs) result(maxstr)
    character(len=strlen), intent(in) :: lhs, rhs
    character(len=strlen) :: maxstr

    if (lhs > rhs) then
      maxstr = lhs 
    else 
      maxstr = rhs
    end if
  end function

  subroutine sub_red(str)
    character(len=:), allocatable :: str(:)

    call co_reduce(str, strmax)
    call check(str, 3)
  end subroutine

  subroutine check(curr, stop_code)
    character(len=*), intent(in) :: curr(:)
    character(len=strlen) :: expect
    integer, intent(in) :: stop_code
    integer :: i

    associate(np => num_images())
      do i = 1, numstrings
        write (expect, "('foo',I04)") i * np
        if (curr(i) /= expect) then
          ! On error print what we got and what we expected.
          print *, this_image(), ": Got: ", curr(i), ", expected: ", expect, ", for i=", i
          stop stop_code
        end if
      end do
    end associate
  end subroutine

end program co_reduce_strings

