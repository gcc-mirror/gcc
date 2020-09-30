! { dg-do run }
!
! Test the fix for PR97045. The report was for the INTEGER version. Testing
! revealed a further bug with the character versions.
!
! Contributed by Igor Gayday  <igor.gayday@mu.edu>
!
program test_prg
  implicit none
  integer :: i
  integer, allocatable :: arr(:, :)
  character(kind = 1, len = 2), allocatable :: chr(:, :)
  character(kind = 4, len = 2), allocatable :: chr4(:, :)

  arr = reshape ([(i, i = 1, 9)], [3, 3])
  do i = 1, 3
    call write_array(arr(1:2, i), i)
  end do

  chr = reshape([(char (i)//char (i+1), i = 65, 83, 2)], [3, 3])
  do i = 1, 3
    call write_array (chr(1:2, i), i)
  end do

  chr4 = reshape([(char (i, kind = 4)//char (i+1, kind = 4), i = 65, 83, 2)], &
                 [3, 3])
  do i = 1, 3
    call write_array (chr4(1:2, i), i)
  end do

contains

  subroutine write_array(array, j)
    class(*), intent(in) :: array(:)
    integer :: i = 2
    integer :: j, k

    select type (elem => array(i))
      type is (integer)
        k = 3*(j-1)+i
        if (elem .ne. k) stop 1
      type is (character(kind = 1, len = *))
        k = 63 + 2*(3*(j-1)+i)
        if (elem .ne. char (k)//char (k+1)) print *, elem, "   ", char (k)//char (k+1)
      type is (character(kind = 4, len = *))
        k = 63 + 2*(3*(j-1)+i)
        if (elem .ne. char (k, kind = 4)//char (k+1, kind = 4)) stop 3
    end select

  end subroutine

end program
