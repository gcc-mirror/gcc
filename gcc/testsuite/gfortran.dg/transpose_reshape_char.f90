! { dg-do run }
! PR fortran/128682
!
! ICE with transpose(reshape()) of character implied-do array constructor
! as actual argument
!
! Contributed by Fred Jones

program p
  implicit none(type, external)
  character(len=6)   :: blk = 'ab#.#.'
  integer, parameter :: w = 2, h = 3
  integer :: i
  character, allocatable :: ref(:,:)
  ref = transpose (reshape ([(blk(i:i), i=1, len(blk))], [w, h]))
  call show ( transpose (reshape ([(blk(i:i), i=1, len(blk))], [w, h])))
  call show ((transpose (reshape ([(blk(i:i), i=1, len(blk))], [w, h]))))
  call show ( reshape ([(blk(i:i), i=1, len(blk))], [h, w], order=[2,1]))
  deallocate (ref)
contains
  subroutine show (g)
    character, intent(in) :: g(:, :)
    if (any (g /= ref)) then
       print '(a)', g
       stop 1
    end if
  end subroutine show
end program p
