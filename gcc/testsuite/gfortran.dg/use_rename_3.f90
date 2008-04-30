! { dg-do compile }
! Tests the fix for PR35997, in which the use association of renamed
! valid2 and flag2 was treated as if the renaming were done on use
! association in the main program.  Thus, the following, direct use
! association of valid and flag did not occur.
!
! Contributed by Drew McCormack <drewmccormack@mac.com>
!
module funcinterfacemod
  interface
    logical function valid ()
    end function
  end interface
  logical :: flag = .true.
end module

module secondmod
  use funcinterfacemod, valid2 => valid, flag2 => flag
end module

logical function valid ()
  valid = .true.
end function

program main
  use secondmod
  use funcinterfacemod
  if (valid ()) then
    print *, 'Is Valid'
  endif
  if (flag) then
    print *, 'Is flag'
  endif
end program
! { dg-final { cleanup-modules "funcinterfacemod secondmod" } }
