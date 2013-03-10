! { dg-do compile }
! Test the fix for PR55362; the error below was missed and an ICE ensued.
!
! ! Contributed by Dominique d'Humieres  <dominiq@lps.ens.fr>
!
program ice_test
  implicit none
  write(*,*) 'message: ', &
             size(Error_Msg),Error_Msg()     ! { dg-error "must be an array" }
  write(*,*) 'message: ', &
             size(Error_Msg ()),Error_Msg()  ! OK of course
contains
  function Error_Msg() result(ErrorMsg)
    character, dimension(:), pointer :: ErrorMsg
    character, dimension(1), target :: str = '!'
    ErrorMsg => str
  end function Error_Msg
end program ice_test
