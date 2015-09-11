! { dg-do compile }
! Just need to check if compiling and linking is possible.
!
! Check that the _vtab linking issue is resolved.
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>

module m
contains
  subroutine fmt()
    class(*), pointer :: arg
    select type (arg)
    type is (integer)
    end select
  end subroutine
end module

program p
  call getSuffix()
contains
  subroutine makeString(arg1)
    class(*) :: arg1
    select type (arg1)
    type is (integer)
    end select
  end subroutine
  subroutine getSuffix()
    call makeString(1)
  end subroutine
end

