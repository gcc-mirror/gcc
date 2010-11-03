! { dg-do run }
! { dg-options "-fbounds-check" }

  character(35) :: nml_contents = "&NMLIST NML_STRING='123456789' /"
  character(4)  :: nml_string
  namelist /nmlist/ nml_string
  nml_string = "abcd"
  read(nml_contents,nml=nmlist)
end program 
! { dg-output "Fortran runtime warning: Namelist object 'nml_string' truncated on read." }
