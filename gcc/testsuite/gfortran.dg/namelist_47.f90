! { dg-do run }

module nml_47
  type             ::  mt
    character(len=2) ::  c012345678901234567890123456789012345678901234567890123456789h(2) = (/"aa","bb"/)
  end type mt
  type             ::  bt
    integer        ::  i(2) = (/1,2/)
    type(mt)       ::  m(2)
  end type bt
end module nml_47

program namelist_47
  use nml_47
  type(bt)         ::  x(2)
  character(140)    ::  teststring
  namelist /mynml/ x

  teststring = " x(2)%m%c012345678901234567890123456789012345678901234567890123456789h(:)(2:2) = 'z','z',"
  call writenml (teststring)
  teststring = " x(2)%m(2)%c012345678901234567890123456789012345678901234567890123456789h(:)(2) = 'z','z',"
  call writenml (teststring)
  teststring = " x(2)%m(2)%c012345678901234567890123456789012345678901234567890123456789h(:)(:3) = 'z','z',"
  call writenml (teststring)
  teststring = " x(2)%m(2)%c012345678901234567890123456789012345678901234567890123456789h(1:2)(k:) = 'z','z',"
  call writenml (teststring)

contains

subroutine writenml (astring)
  character(140), intent(in)  :: astring
  character(300)   :: errmessage
  integer          :: ierror

  open (10, status="scratch", delim='apostrophe')
  write (10, '(A)') "&MYNML"
  write (10, '(A)') astring
  write (10, '(A)') "/"
  rewind (10)
  read (10, nml = mynml, iostat=ierror, iomsg=errmessage)
  if (ierror == 0) STOP 1
  print '(a)', trim(errmessage)
  close (10)

end subroutine writenml

end program namelist_47
! { dg-output "Multiple sub-objects with non-zero rank in namelist object x%m%c012345678901234567890123456789012345678901234567890123456789h(\n|\r\n|\r)" }
! { dg-output "Missing colon in substring qualifier for namelist variable x%m%c012345678901234567890123456789012345678901234567890123456789h(\n|\r\n|\r)" }
! { dg-output "Substring out of range for namelist variable x%m%c012345678901234567890123456789012345678901234567890123456789h(\n|\r\n|\r)" }
! { dg-output "Bad character in substring qualifier for namelist variable x%m%c012345678901234567890123456789012345678901234567890123456789h(\n|\r\n|\r)" }
