! { dg-do run }
!
! Test the fix for PR105054.
!
! Contributed by Arjen Markus  <arjen.markus895@gmail.com>
!
module string_pointers
    implicit none
    character(len=20), dimension(10), target :: array_strings
    character(len=:), dimension(:), target, allocatable :: array_strings2

contains

function pointer_to_string( i , flag)
     integer, intent(in) :: i, flag

     character(len=:), pointer :: pointer_to_string

     if (flag == 1) then
       pointer_to_string => array_strings(i)
       return
     endif

     if (.not.allocated (array_strings2)) allocate (array_strings2(4), &
                                                    mold = '        ')
     pointer_to_string => array_strings2(i)
end function pointer_to_string

function pointer_to_string2( i , flag) result (res)
     integer, intent(in) :: i, flag

     character(len=:), pointer :: res

     if (flag == 1) then
       res => array_strings(i)
       return
     endif

     if (.not.allocated (array_strings2)) allocate (array_strings2(4), &
                                                    mold = '        ')
     res => array_strings2(i)
end function pointer_to_string2

end module string_pointers

program chk_string_pointer
    use string_pointers
    implicit none
    integer :: i
    character(*), parameter :: chr(4) = ['1234      ','ABCDefgh  ', &
                                         '12345678  ','          ']

    pointer_to_string(1, 1) = '1234567890'
    pointer_to_string(2, 1) = '12345678901234567890'

    if (len(pointer_to_string(3, 1)) /= 20) stop 1

    array_strings(1) = array_strings(1)(1:4) // 'ABC'
    if (pointer_to_string(1, 1) /= '1234ABC') stop 2

    pointer_to_string(1, 2) = '1234'
    pointer_to_string(2, 2) = 'ABCDefgh'
    pointer_to_string(3, 2) = '12345678'

    do i = 1, 3
      if (trim (array_strings2(i)) /= trim(chr(i))) stop 3
    enddo

! Clear the target arrays
    array_strings = repeat (' ', 20)
    deallocate (array_strings2)

! Repeat with an explicit result.
    pointer_to_string2(1, 1) = '1234567890'
    pointer_to_string2(2, 1) = '12345678901234567890'

    if (len(pointer_to_string(3, 1)) /= 20) stop 4

    array_strings(1) = array_strings(1)(1:4) // 'ABC'
    if (pointer_to_string(1, 1) /= '1234ABC') stop 5

    pointer_to_string2(1, 2) = '1234'
    pointer_to_string2(2, 2) = 'ABCDefgh'
    pointer_to_string2(3, 2) = '12345678'

    do i = 1, 3
      if (trim (array_strings2(i)) /= trim(chr(i))) stop 6
    enddo
end program chk_string_pointer
