! { dg-do compile }
! PR 96073 - this used to cause an ICE.
! Test case by Jürgen Reuter.

module m
  implicit none
  private

  interface
     subroutine GetXminM (set, xmin)
       integer, intent(in) :: set
       real, intent(out) :: xmin
     end subroutine GetXminM
  end interface
  interface 
     subroutine foo(a)  ! { dg-warning "Type mismatch" }
       integer, intent(in) :: a 
     end subroutine foo
  end interface

contains

  subroutine s ()
    real :: xmin
    integer :: set
    external :: GetXminM, foo
    call GetXminM (set, xmin)
    call foo(1.0) ! { dg-warning "Type mismatch" }
  end subroutine s

end module m
