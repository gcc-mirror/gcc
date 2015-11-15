! { dg-do run }
!
! Tests the fix for PR50221
!
! Contributed by Clive Page  <clivegpage@gmail.com>
!            and Tobias Burnus  <burnus@gcc.gnu.org>
!
! This is from comment #2 by Tobias Burnus.
!
module m
  character(len=:), save, allocatable :: str(:)
  character(len=2), parameter :: const(3) = ["a1", "b2", "c3"]
end

  use m
  call test()
  if(allocated(str)) deallocate(str)
  call foo
contains
  subroutine test()
    call doit()
!    print *, 'strlen=',len(str),' / array size =',size(str)
!    print '(3a)', '>',str(1),'<'
!    print '(3a)', '>',str(2),'<'
!    print '(3a)', '>',str(3),'<'
    if (any (str .ne. const)) call abort
  end subroutine test
  subroutine doit()
    str = const
  end subroutine doit
  subroutine foo
!
! This is the original PR from Clive Page
!
    character(:), allocatable, dimension(:) :: array
    array = (/'xx', 'yy', 'zz'/)
!    print *, 'array=', array, len(array(1)), size(array)
    if (any (array .ne. ["xx", "yy", "zz"])) call abort
  end subroutine
end
