! { dg-do run }
! PR36657 Namelist string constant immediately followed by comment
program gfcbug79
  implicit none
  integer, parameter :: nnml = 10
  character(len=8)  :: model = ""
  namelist /NML/       model
  open (nnml, status="scratch")
  write(nnml,*) "&nml! This is a just comment"
  write(nnml,*) "  model='foo'! This is a just comment"
  write(nnml,*) "/"
  rewind(nnml)
  read (nnml, nml=NML)
  if (model /= 'foo') call abort
  close(nnml)
end program gfcbug79
