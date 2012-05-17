! { dg-do compile }
! { dg-options "-std=legacy" }
!
! Test for the fix to PR24409 - the name clash between the module
! name and the interface formal argument would cause an ICE.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module string 
  interface
    function  lc(string )
      character(len=*), intent(in)  :: string 
      character(len=len(string ))    :: lc
    end function lc
  end interface
end module string

module serial
  use string
end module serial

  use serial
  use string
  character*15  :: buffer  
  buffer = lc ("Have a Nice DAY")
  end
