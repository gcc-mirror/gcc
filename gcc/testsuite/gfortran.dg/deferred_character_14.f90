! { dg-do run }
!
! Test fix for PR60795 comments #1 and  #4
!
! Contributed by Kergonath  <kergonath@me.com>
!
module m
contains
    subroutine allocate_array(s_array)
        character(:), dimension(:), allocatable, intent(out) :: s_array

        allocate(character(2) :: s_array(2))
        s_array = ["ab","cd"]
    end subroutine
end module

program stringtest
    use m
    character(:), dimension(:), allocatable :: s4
    character(:), dimension(:), allocatable :: s
! Comment #1
    allocate(character(1) :: s(10))
    if (size (s) .ne. 10) STOP 1
    if (len (s) .ne. 1) STOP 2
! Comment #4
    call allocate_array(s4)
    if (size (s4) .ne. 2) STOP 3
    if (len (s4) .ne. 2) STOP 4
    if (any (s4 .ne. ["ab", "cd"])) STOP 5
 end program
