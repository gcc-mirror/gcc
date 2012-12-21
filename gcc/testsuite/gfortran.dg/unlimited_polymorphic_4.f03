! { dg-do compile }
!
! Fix PR55763
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
module mpi_f08_f
  implicit none
  abstract interface
    subroutine user_function( inoutvec )
      class(*), dimension(:), intent(inout) :: inoutvec
    end subroutine user_function
  end interface
end module

module mod_test1
  use mpi_f08_f
  implicit none
contains
  subroutine my_function( invec )   ! { dg-error "no IMPLICIT type" }
    class(*), dimension(:), intent(inout) :: inoutvec    ! { dg-error "not a DUMMY" }

    select type (inoutvec)
    type is (integer)
         inoutvec = 2*inoutvec
    end select
  end subroutine my_function
end module

module mod_test2
  use mpi_f08_f
  implicit none
contains
  subroutine my_function( inoutvec )  ! Used to produce a BOGUS ERROR
    class(*), dimension(:), intent(inout) :: inoutvec

    select type (inoutvec)
    type is (integer)
         inoutvec = 2*inoutvec
    end select
  end subroutine my_function
end module
