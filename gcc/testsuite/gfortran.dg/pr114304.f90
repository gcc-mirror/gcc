! { dg-do run }
!
! PR fortran/114304
!
! See also PR fortran/105473
!
! Testing: Does list-directed reading an integer/real allow some non-integer input?
!
! Note: GCC result comments before fix of this PR.

  implicit none
  call t(.true.,  'comma', ';') ! No error shown
  call t(.false., 'point', ';') ! /!\ gfortran: no error, others: error
  call t(.false., 'comma', ',') ! Error shown
  call t(.true.,  'point', ',') ! No error shown
  call t(.false., 'comma', '.') ! Error shown
  call t(.false., 'point', '.') ! Error shown
  call t(.false., 'comma', '5.') ! Error shown
  call t(.false., 'point', '5.') ! gfortran/flang: Error shown, ifort: no error
  call t(.false., 'comma', '5,') ! gfortran: error; others: no error
  call t(.true.,  'point', '5,') ! No error shown
  call t(.true.,  'comma', '5;') ! No error shown
  call t(.false., 'point', '5;') ! /!\ gfortran: no error shown, others: error
  call t(.true.,  'comma', '7 .') ! No error shown
  call t(.true.,  'point', '7 .') ! No error shown
  call t(.true.,  'comma', '7 ,') ! /!\ gfortran: error; others: no error
  call t(.true.,  'point', '7 ,') ! No error shown
  call t(.true.,  'comma', '7 ;') ! No error shown
  call t(.true.,  'point', '7 ;') ! No error shown

!  print *, '---------------'

  call t(.false., 'comma', '8.', .true.) ! Error shown
  call t(.true.,  'point', '8.', .true.) ! gfortran/flang: Error shown, ifort: no error
  call t(.true.,  'comma', '8,', .true.) ! gfortran: error; others: no error
  call t(.true.,  'point', '8,', .true.) ! No error shown
  call t(.true.,  'comma', '8;', .true.) ! No error shown
  call t(.false., 'point', '8;', .true.) ! /!\ gfortran: no error shown, others: error
  call t(.true.,  'comma', '9 .', .true.) ! No error shown
  call t(.true.,  'point', '9 .', .true.) ! No error shown
  call t(.true.,  'comma', '9 ,', .true.) ! /!\ gfortran: error; others: no error
  call t(.true.,  'point', '9 ,', .true.) ! No error shown
  call t(.true.,  'comma', '9 ;', .true.) ! No error shown
  call t(.true.,  'point', '9 ;', .true.) ! No error shown
  call t(.false., 'comma', '3,3.', .true.) ! Error shown
  call t(.false., 'point', '3.3.', .true.) ! Error shown
  call t(.false., 'comma', '3,3,', .true.) ! gfortran/flang: no error; ifort: error
  call t(.true.,  'comma', '3,3;', .true.) ! No error shown
  call t(.false., 'point', '3.3;', .true.) ! gfortran/flang: no error; ifort: error
  call t(.true.,  'comma', '4,4 .', .true.) ! N error shown
  call t(.true.,  'point', '4.4 .', .true.) ! No error shown
  call t(.true.,  'comma', '4,4 ,', .true.) ! /!\ gfortran: error; others: no error
  call t(.true.,  'point', '4.4 ,', .true.) ! No error shown
  call t(.true.,  'comma', '4,4 ;', .true.) ! No error shown
  call t(.true.,  'point', '4.4 ;', .true.) ! No error shown

!  print *, '---------------'

  call t(.true.,  'comma', '8', .true.)
  call t(.true.,  'point', '8', .true.)
  call t(.true.,  'point', '9 ;', .true.)
  call t(.true.,  'comma', '3;3.', .true.)
  call t(.true.,  'point', '3,3.', .true.)
  call t(.true.,  'comma', '3;3,', .true.)
  call t(.true.,  'comma', '3;3;', .true.)
  call t(.true.,  'point', '3,3;', .true.)
  call t(.true.,  'comma', '4;4 .', .true.)
  call t(.true.,  'point', '4,4 .', .true.)
  call t(.true.,  'comma', '4;4 ,', .true.)
  call t(.true.,  'point', '4,4 ,', .true.)
  call t(.true.,  'comma', '4;4 ;', .true.)
  call t(.true.,  'point', '4,4 ;', .true.)

  call t2('comma', ',2')
  call t2('point', '.2')
  call t2('comma', ',2;')
  call t2('point', '.2,')
  call t2('comma', ',2 ,')
  call t2('point', '.2 .')
contains
subroutine t2(dec, testinput)
  character(*) :: dec, testinput
  integer ios
  real :: r
  r = 42
  read(testinput,*,decimal=dec, iostat=ios) r
  if (ios /= 0 .or.  abs(r - 0.2) > epsilon(r)) then
    stop 3 
  end if
end
subroutine t(valid, dec, testinput, isreal)
  logical, value :: valid
  character(len=*) :: dec, testinput
  logical, optional :: isreal
  logical :: isreal2
  integer n,ios
  real :: r
  r = 42; n = 42
  isreal2 = .false.
  if (present(isreal)) isreal2 = isreal

  if (isreal2) then
    read(testinput,*,decimal=dec,iostat=ios) r
    if ((valid .and. ios /= 0) .or. (.not.valid .and. ios == 0)) then
      stop 1
    end if
  else
    read(testinput,*,decimal=dec,iostat=ios) n
    if ((valid .and. ios /= 0) .or. (.not.valid .and. ios == 0)) then
      stop 1
    end if
  end if
end
end program
