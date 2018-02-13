! { dg-do compile }

! PR fortran/36592
!
! Procedure Pointers inside COMMON blocks.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>.

abstract interface
 subroutine foo() bind(C)
 end subroutine foo
end interface

procedure(foo), pointer, bind(C) :: proc
common /com/ proc,r  ! { dg-error "PROCEDURE attribute conflicts with COMMON attribute" }

common s
call s()  ! { dg-error "PROCEDURE attribute conflicts with COMMON attribute" }

end 
