! { dg-do compile }
! PR fortran/82480 - checking of arguments to STAT/LSTAT/FSTAT

subroutine sub1 ()
  integer, parameter  :: ik = kind(1)
  integer(ik)         :: buff12(12)
  integer(ik)         :: buff13(13)
  integer(ik)         :: unit = 10
  integer(ik)         :: ierr
  character(len=64)   :: name = "/etc/passwd"
  ierr = stat  (name, values= buff12)  ! { dg-error "too small" }
  ierr = stat  (name, values= buff13)
  ierr = lstat (name, values= buff12)  ! { dg-error "too small" }
  ierr = lstat (name, values= buff13)
  ierr = fstat (unit, values= buff12)  ! { dg-error "too small" }
  ierr = fstat (unit, values= buff13)
  ierr = stat  (name, values=(buff13)) ! { dg-error "must be a variable" }
  ierr = lstat (name, values=(buff13)) ! { dg-error "must be a variable" }
  ierr = fstat (unit, values=(buff13)) ! { dg-error "must be a variable" }
end

subroutine sub2 ()
  integer, parameter  :: ik = kind(1)
  integer(ik)         :: buff12(12)
  integer(ik), target :: buff13(13) = 0
  integer(ik)         :: unit = 10
  integer(ik), target :: ierr = 0
  character(len=64)   :: name = "/etc/passwd"
  integer(ik),pointer :: pbuf(:) => buff13
  integer(ik),pointer :: perr    => ierr
  call stat  (name, status=ierr, values= buff12)  ! { dg-error "too small" }
  call stat  (name, status=ierr, values= buff13)
  call lstat (name, status=ierr, values= buff12)  ! { dg-error "too small" }
  call lstat (name, status=ierr, values= buff13)
  call fstat (unit, status=ierr, values= buff12)  ! { dg-error "too small" }
  call fstat (unit, status=ierr, values= buff13)
  call stat  (name, status=ierr, values=(buff13)) ! { dg-error "must be a variable" }
  call lstat (name, status=ierr, values=(buff13)) ! { dg-error "must be a variable" }
  call fstat (unit, status=ierr, values=(buff13)) ! { dg-error "must be a variable" }
  call stat  (name, status=(ierr),values=buff13)  ! { dg-error "must be a variable" }
  call lstat (name, status=(ierr),values=buff13)  ! { dg-error "must be a variable" }
  call fstat (unit, status=(ierr),values=buff13)  ! { dg-error "must be a variable" }
  call stat  (name, status=perr, values= pbuf)
  call lstat (name, status=perr, values= pbuf)
  call fstat (unit, status=perr, values= pbuf)
end
