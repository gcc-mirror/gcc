! { dg-do compile }
! { dg-options "-std=legacy" }
!
! Test for supplementary fix to PR24409 - the name clash between the module
! variable and the interface formal argument would cause an ICE.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module anything 
  interface
    function  lc(string )
      character(len=*), intent(in)  :: string 
      character(len=len(string ))    :: lc
    end function lc
  end interface
  character(len=12) :: string
end module anything

module serial
  use anything
end module serial

  use serial
  use anything
  character*15  :: buffer  
  buffer = lc ("Have a Nice DAY")
  end

! { dg-final { cleanup-modules "anything serial" } }
