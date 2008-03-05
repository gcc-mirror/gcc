!{ dg-do run { target fd_truncate } }
! Tests various combinations of intrinsic types, derived types, arrays,
! dummy arguments and common to check nml_get_addr_expr in trans-io.c.
! See comments below for selection.
! provided by Paul Thomas - pault@gcc.gnu.org

module global
  type             ::  mt
    sequence
    integer        ::  ii(4)
  end type mt
end module global

program namelist_14
  use global
  common /myc/ cdt
  integer          ::  i(2) = (/101,201/)
  type(mt)         ::  dt(2)
  type(mt)         ::  cdt
  real(kind=8)           ::  pi = 3.14159_8
  character*10     ::  chs="singleton"
  character*10     ::  cha(2)=(/"first     ","second    "/)

  dt = mt ((/99,999,9999,99999/))
  cdt = mt ((/-99,-999,-9999,-99999/))
  call foo (i,dt,pi,chs,cha)

contains

  logical function dttest (dt1, dt2)
    use global
    type(mt)       :: dt1
    type(mt)       :: dt2
    dttest = any(dt1%ii == dt2%ii)
  end function dttest


  subroutine foo (i, dt, pi, chs, cha)
    use global
    common /myc/ cdt
    real(kind=8)        :: pi                   !local real scalar
    integer        :: i(2)                 !dummy arg. array
    integer        :: j(2) = (/21, 21/)    !equivalenced array
    integer        :: jj                   !    -||-     scalar
    integer        :: ier
    type(mt)       :: dt(2)                !dummy arg., derived array
    type(mt)       :: dtl(2)               !in-scope derived type array
    type(mt)       :: dts                  !in-scope derived type
    type(mt)       :: cdt                  !derived type in common block
    character*10   :: chs                  !dummy arg. character var.
    character*10   :: cha(:)               !dummy arg. character array
    character*10   :: chl="abcdefg"        !in-scope character var.
    equivalence (j,jj)
    namelist /z/     dt, dtl, dts, cdt, j, jj, i, pi, chs, chl, cha

    dts = mt ((/1, 2, 3, 4/))
    dtl = mt ((/41, 42, 43, 44/))

    open (10, status = "scratch", delim='apostrophe')
    write (10, nml = z, iostat = ier)
    if (ier /= 0 ) call abort()
    rewind (10)

    i = 0
    j = 0
    jj = 0
    pi = 0
    dt  = mt ((/0, 0, 0, 0/))
    dtl = mt ((/0, 0, 0, 0/))
    dts = mt ((/0, 0, 0, 0/))
    cdt = mt ((/0, 0, 0, 0/))
    chs = ""
    cha = ""
    chl = ""

    read (10, nml = z, iostat = ier)
    if (ier /= 0 ) call abort()
    close (10)

    if (.not.(dttest (dt(1),  mt ((/99,999,9999,99999/))) .and.  &
          dttest (dt(2),  mt ((/99,999,9999,99999/))) .and.  &
          dttest (dtl(1), mt ((/41, 42, 43, 44/))) .and.     &
          dttest (dtl(2), mt ((/41, 42, 43, 44/))) .and.     &
          dttest (dts, mt ((/1, 2, 3, 4/))) .and.            &
          dttest (cdt, mt ((/-99,-999,-9999,-99999/))) .and. &
          all (j ==(/21, 21/)) .and.                         &
          all (i ==(/101, 201/)) .and.                       &
          (pi == 3.14159_8) .and.                            &
          (chs == "singleton") .and.                         &
          (chl == "abcdefg") .and.                           &
          (cha(1)(1:10) == "first    ") .and.                &
          (cha(2)(1:10) == "second    "))) call abort ()

    end subroutine foo
end program namelist_14 

! { dg-final { cleanup-modules "global" } }
