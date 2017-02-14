! { dg-do compile }
! { dg-options "-fdec-structure" }
!
! PR fortran/77764
!
! Test an ICE due to a map with zero components.
!

program p

structure /s1/
  union
    map
    end map
    map
      real :: a = 2.0
    end map
  end union
end structure

end
