! { dg-do run }
!
! Test the fix for PR82923, in which an ICE occurred because the
! character length from 'getchars' scope was being used in the
! automatic allocation of 'mine'.
!
! Contributed by "Werner Blokbuster"  <werner.blokbuster@gmail.com>
!
module m
    implicit none
contains
    function getchars(my_len,my_size)
        integer, intent(in) :: my_len, my_size
        character(my_len) :: getchars(my_size)
            getchars = 'A-'
    end function getchars

    function getchars2(my_len)
        integer, intent(in) :: my_len
        character(my_len) :: getchars2
            getchars2 = 'B--'
    end function getchars2
end module m

program testca
    use m, only: getchars, getchars2
    implicit none
    character(:), allocatable :: mine(:)
    character(:), allocatable :: mine2
    integer :: i

    ! ICE occured at this line:
    mine = getchars(2,4)
    if (any (mine .ne. [('A-', i = 1, 4)])) stop 1

    ! The scalar version was fine and this will keep it so:
    mine2 = getchars2(3)
    if (mine2 .ne. 'B--') stop 2
end program testca
