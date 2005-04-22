!{ dg-do run }
! Tests simple derived types.
! Provided by Paul Thomas - pault@gcc.gnu.org

program namelist_13

  type                        ::      yourtype
    integer, dimension(2)     ::      yi = (/8,9/)
    real, dimension(2)        ::      yx = (/80.,90./)
    character(len=2)          ::      ych = "xx"
  end type yourtype

  type                        ::      mytype
    integer, dimension(2)     ::      myi = (/800,900/)
    real, dimension(2)        ::      myx = (/8000.,9000./)
    character(len=2)          ::      mych = "zz"
    type(yourtype)            ::      my_yourtype
  end type mytype

  type(mytype)                ::      z
  integer                     ::      ier
  integer                     ::      zeros(10)
  namelist /mynml/ zeros, z

  zeros = 0
  zeros(5) = 1

  open(10,status="scratch")
  write (10, nml=mynml, iostat=ier)
  if (ier.ne.0) call abort

  rewind (10)
  read (10, NML=mynml, IOSTAT=ier)
  if (ier.ne.0) call abort
  close (10)

end program namelist_13

