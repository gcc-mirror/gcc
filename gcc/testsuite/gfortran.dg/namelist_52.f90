! { dg-do run }
! PR36582 Namelist I/O error: Bogus "Cannot match namelist object"
! Test case derived from PR.
module mod1

type screen_io_type
integer :: begin
end type screen_io_type

type adjoint_type
type(screen_io_type) :: screen_io_fs_ntime
character(12) :: solver_type
end type adjoint_type

type(adjoint_type) :: adjoint
namelist/info_adjoint/adjoint

end module mod1

program gfortran_error_2
use mod1
adjoint%solver_type = "abcdefghijkl"
open(31,status='scratch')
write(31, '(a)') "&info_adjoint"
write(31, '(a)') "adjoint%solver_type = 'direct'"
write(31, '(a)') "adjoint%screen_io_fs_ntime%begin = 42"
write(31, '(a)') "/"
rewind(31)
read(31,nml=info_adjoint)
if (adjoint%solver_type /= 'direct') call abort
if (adjoint%screen_io_fs_ntime%begin /= 42) call abort
end program gfortran_error_2
