! ICE with gfortran 4.5 at -O1:
!gfcbug98.f90: In function ‘convert_cof’:
!gfcbug98.f90:36:0: internal compiler error: in pt_solutions_same_restrict_base,
!at tree-ssa-structalias.c:5072
module foo
  implicit none
  type t_time
     integer :: secs = 0
  end type t_time
contains
  elemental function time_cyyyymmddhh (cyyyymmddhh) result (time)
    type (t_time)                :: time
    character(len=10),intent(in) :: cyyyymmddhh
  end function time_cyyyymmddhh

  function nf90_open(path, mode, ncid)
    character(len = *), intent(in) :: path
    integer, intent(in)  :: mode
    integer, intent(out) :: ncid
    integer              :: nf90_open
  end function nf90_open
end module foo
!==============================================================================
module gfcbug98
  use foo
  implicit none

  type t_fileinfo
     character(len=10) :: atime = ' '
  end type t_fileinfo

  type t_body
     real         :: bg(10)
  end type t_body
contains
  subroutine convert_cof (ifile)
    character(len=*) ,intent(in) :: ifile

    character(len=5)         :: version
    type(t_fileinfo)         :: gattr
    type(t_time)             :: atime
    type(t_body),allocatable :: tmp_dat(:)
    real        ,allocatable :: BDA(:, :, :)

    call open_input
    call convert_data
  contains
    subroutine open_input
      integer             :: i,j
      version = ''
      j = nf90_open(ifile, 1, i)
    end subroutine open_input
    !--------------------------------------------------------------------------
    subroutine convert_data
      BDA(1,:,1) = tmp_dat(1)% bg(:)
      atime = time_cyyyymmddhh (gattr% atime)
    end subroutine convert_data
  end subroutine convert_cof
end module gfcbug98
