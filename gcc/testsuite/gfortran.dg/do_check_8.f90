! { dg-do compile }
! PR 55593 - bogus error with generic subroutines
module foo
  implicit none
  interface sub
     subroutine sub2(i)
       integer, intent(in) :: i
     end subroutine sub2
     subroutine sub(i)
       integer, dimension(:), intent(out) :: i
     end subroutine sub
  end interface sub

  interface tub2
     subroutine tub2(i)
       integer, intent(in) :: i
     end subroutine tub2
     subroutine tub(i)
       integer, dimension(:), intent(out) :: i
     end subroutine tub
  end interface tub2

  interface func
     integer function ifunc(i)
       integer, intent(in) :: i
     end function ifunc
     integer function func(i)
       integer, intent(in) :: i(:)
     end function func
  end interface func

  interface igunc
     integer function igunc(i)
       integer, intent(in) :: i
     end function igunc
     integer function gunc(i)
       integer, intent(in) :: i(:)
     end function gunc
  end interface igunc
end module foo

program main
  use foo
  implicit none
  integer :: i
  do i=1,10
     call sub(i)
     call tub2(i)
  end do
  do i=1,10
     print *,func(i)
     print *,igunc(i)
  end do

  do undeclared=1,10        ! { dg-error "has no IMPLICIT type" }
     call sub(undeclared)
  end do
end program main
