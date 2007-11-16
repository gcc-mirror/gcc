! { dg-do run }
! Tests the fix for PR33986, in which the call to scram would call
! an ICE because allocatable result actuals had not been catered for.
!
!  Contributed by Damian Rouson <damian@rouson.net>
!
function transform_to_spectral_from() result(spectral)
  integer, allocatable :: spectral(:)
  allocate(spectral(2))
  call scram(spectral)
end function transform_to_spectral_from

subroutine scram (x)
  integer x(2)
  x = (/1,2/)
end subroutine

  interface
    function transform_to_spectral_from() result(spectral)
      integer, allocatable :: spectral(:)
    end function transform_to_spectral_from
  end interface
  if (any (transform_to_spectral_from () .ne. (/1,2/))) call abort ()
end
