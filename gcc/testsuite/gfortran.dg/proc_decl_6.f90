! { dg-do compile }
! PR fortran/33945
!
! MODULE PROCEDURE in the interface was wrongly accepted
module modproc2
  implicit none
  interface
    subroutine x
    end subroutine x
  end interface
  procedure(x) :: y
  interface bar
    module procedure y ! { dg-error "not a module procedure" }
  end interface bar
end module modproc2

end
