! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
! PR fortran/82480 - make STAT/LSTAT/FSTAT generic

subroutine fstat_sub_wrapper (unit, values8, status, opt_status4, opt_status8)
  implicit none
  integer(1), intent(in)            :: unit
  integer(8), intent(out)           :: values8(:)
  integer(2), intent(out)           :: status
  integer(4), intent(out), optional :: opt_status4
  integer(8), intent(out), optional :: opt_status8
  call fstat (unit, values8, status)
  call fstat (unit, values8, opt_status4)
  call fstat (unit, values8, opt_status8)
end

subroutine stat_sub_wrapper (name, values4, status, opt_status4, opt_status8)
  implicit none
  character(*), intent(in)            :: name
  integer(4),   intent(out)           :: values4(:)
  integer(2),   intent(out)           :: status
  integer(4),   intent(out), optional :: opt_status4
  integer(8),   intent(out), optional :: opt_status8
  call stat  (name, values4, status)
  call lstat (name, values4, status)
  call stat  (name, values4, opt_status4)
  call lstat (name, values4, opt_status4)
  call stat  (name, values4, opt_status8)
  call lstat (name, values4, opt_status8)
end

subroutine sub1 ()
  implicit none
  character(len=32)   :: name = "/etc/passwd"
  integer(1)          :: unit1 = 10
  integer(4)          :: unit4 = 10, buff4(13)
  integer(8)          :: unit8 = 10, buff8(13)
  integer             :: ierr
  ierr = fstat (unit1, values=buff4)
  ierr = fstat (unit1, values=buff8)
  ierr = fstat (unit4, values=buff4)
  ierr = fstat (unit4, values=buff8)
  ierr = fstat (unit8, values=buff4)
  ierr = fstat (unit8, values=buff8)
  ierr = stat  (name,  values=buff4)
  ierr = stat  (name,  values=buff8)
  ierr = lstat (name,  values=buff4)
  ierr = lstat (name,  values=buff8)
end

subroutine sub2 ()
  implicit none
  integer(2)          :: ierr2, unit2 = 10
  integer(4)          :: ierr4, unit4 = 10, buff4(13)
  integer(8)          :: ierr8, unit8 = 10, buff8(13)
  character(len=32)   :: name = "/etc/passwd"
  call fstat (unit2, values=buff4)
  call fstat (unit2, values=buff8)
  call fstat (unit4, values=buff4)
  call fstat (unit4, values=buff8)
  call fstat (unit8, values=buff4)
  call fstat (unit8, values=buff8)
  call stat  (name,  values=buff4)
  call lstat (name,  values=buff4)
  call stat  (name,  values=buff8)
  call lstat (name,  values=buff8)
  call fstat (unit4, values=buff4, status=ierr2)
  call fstat (unit4, values=buff4, status=ierr4)
  call fstat (unit4, values=buff4, status=ierr8)
  call fstat (unit4, values=buff8, status=ierr2)
  call fstat (unit4, values=buff8, status=ierr4)
  call fstat (unit4, values=buff8, status=ierr8)
  call stat  (name,  values=buff4, status=ierr4)
  call lstat (name,  values=buff4, status=ierr4)
  call stat  (name,  values=buff4, status=ierr8)
  call lstat (name,  values=buff4, status=ierr8)
  call stat  (name,  values=buff8, status=ierr4)
  call lstat (name,  values=buff8, status=ierr4)
end

! { dg-final { scan-tree-dump-times "_gfortran_fstat_i4_sub" 6 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_lstat_i4_sub" 6 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_stat_i4_sub" 6 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_fstat_i8_sub" 9 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_lstat_i8_sub" 2 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_stat_i8_sub" 2 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_fstat_i4 " 3 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_fstat_i8 " 3 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_lstat_i4 " 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_lstat_i8 " 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_stat_i4 " 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_stat_i8 " 1 "original" } }
! { dg-final { scan-tree-dump-times "opt_status4" 11 "original" } }
! { dg-final { scan-tree-dump-times "opt_status8" 11 "original" } }
