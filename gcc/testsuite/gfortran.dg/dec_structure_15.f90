! { dg-do "compile" }
! { dg-options "" }
!
! PR fortran/77584
! Regression where "structure" and "record" greedily matched a
! declaration-type-spec in a procedure-declaration-statement (R1212).
!
module dec_structure_15
  abstract interface
     double precision function structure_()
     end function structure_
  end interface
  abstract interface
     double precision function record_()
     end function record_
  end interface
contains
  double precision function a()
    procedure(structure_), pointer :: b ! regression: Unclassifiable statement
    a = 0.0
  end function
  double precision function a2()
    procedure(record_), pointer :: b ! regression: Unclassifiable statement
    a2 = 0.0
  end function
end module
! { dg-final { cleanup-modules "dec_structure_15" } }
