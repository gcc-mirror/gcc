!{ dg-do run }
! Tests arrays of derived types containing derived type arrays whose
! components are character arrays - exercises object name parser in
! list_read.c. Checks that namelist output can be reread. 
! provided by Paul Thomas - pault@gcc.gnu.org

module global
  type             ::  mt
    character(len=2) ::  ch(2) = (/"aa","bb"/)
  end type mt
  type             ::  bt
    integer        ::  i(2) = (/1,2/)
    type(mt)       ::  m(2)
  end type bt
end module global

program namelist_15
  use global
  type(bt)         ::  x(2)

  namelist /mynml/ x

  open (10, status = "scratch")
  write (10, '(A)') "&MYNML"
  write (10, '(A)') " x = 3, 4, 'dd', 'ee', 'ff', 'gg',"
  write (10, '(A)') "     4, 5, 'hh', 'ii', 'jj', 'kk',"
  write (10, '(A)') " x%i = , ,-3, -4"
  write (10, '(A)') " x(2)%m(1)%ch(2) ='q',"
  write (10, '(A)') " x(2)%m(2)%ch(1)(1) ='w',"
  write (10, '(A)') " x%m%ch(:)(2) = 'z','z','z','z','z','z','z','z',"
  write (10, '(A)') "&end"
   
  rewind (10)
  read (10, nml = mynml, iostat = ier)
  if (ier .ne. 0) print *, 'First read.' !call abort () 
  close (10)

  open (10, status = "scratch", delim='apostrophe')
  write (10, nml = mynml)
  rewind (10)

  read (10, nml = mynml, iostat = ier)
  if (ier .ne. 0) print *, 'Second read.' !call abort () 
  close(10)

  if (.not. ((x(1)%i(1) == 3)          .and. &
             (x(1)%i(2) == 4)          .and. &
             (x(1)%m(1)%ch(1) == "dz") .and. &
             (x(1)%m(1)%ch(2) == "ez") .and. &
             (x(1)%m(2)%ch(1) == "fz") .and. &
             (x(1)%m(2)%ch(2) == "gz") .and. &
             (x(2)%i(1) == -3)         .and. &
             (x(2)%i(2) == -4)         .and. &
             (x(2)%m(1)%ch(1) == "hz") .and. &
             (x(2)%m(1)%ch(2) == "qz") .and. &
             (x(2)%m(2)%ch(1) == "wz") .and. &
             (x(2)%m(2)%ch(2) == "kz"))) call abort ()

end program namelist_15

! { dg-final { cleanup-modules "global" } }
