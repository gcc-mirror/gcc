! { dg-do compile }
!
! PR fortran/47203
! The USE statement of a module was not rejected in a procedure with the same
! name if the procedure was contained.
!
! Contributed by Tobias Burnus <burnus@net-b.de>

module m
end module m

call m
contains
  subroutine m()
     use m      ! { dg-error "is also the name of the current program unit" }
  end subroutine m
end

