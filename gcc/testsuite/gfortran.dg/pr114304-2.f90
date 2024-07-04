! { dg-do run }
!
! PR fortran/114304
!
! Ensure that '\t' (tab) is supported as separator in list-directed input
! While not really standard conform, this is widely used in user input and
! widely supported.
!

use iso_c_binding
implicit none
character(len=*,kind=c_char), parameter :: tab = C_HORIZONTAL_TAB

! Accept '<tab>' as variant to ' ' as separator
! Check that <carriage_return><new line> and <new_line> are handled

character(len=*,kind=c_char), parameter :: nml_str &
   = '&inparm'//C_CARRIAGE_RETURN // C_NEW_LINE // &
     'first'//tab//'='//tab//' .true.'// C_NEW_LINE // &
     ' , other'//tab//' ='//tab//'3'//tab//', 2'//tab//'/'

! Check that <carriage_return> is handled,

! Note: For new line, Unix uses \n, Windows \r\n but old Apple systems used '\r'
!
! Gfortran does not seem to support all \r, but the following is supported
! since ages, ! which seems to be a gfortran extension as ifort and flang don't like it.

character(len=*,kind=c_char), parameter :: nml_str2 &
   = '&inparm'//C_CARRIAGE_RETURN // C_NEW_LINE // &
     'first'//C_NEW_LINE//'='//tab//' .true.'// C_CARRIAGE_RETURN // &
     ' , other'//tab//' ='//tab//'3'//tab//', 2'//tab//'/'

character(len=*,kind=c_char), parameter :: str &
   = tab//'1'//tab//'2,'//tab//'3'//tab//',4'//tab//','//tab//'5'//tab//'/'
character(len=*,kind=c_char), parameter :: str2 &
   = tab//'1'//tab//'2;'//tab//'3'//tab//';4'//tab//';'//tab//'5'//tab//'/'
logical :: first
integer :: other(4)
integer :: ints(6)
namelist /inparm/ first , other

other = 1

open(99, file="test.inp")
write(99, '(a)') nml_str
rewind(99)
read(99,nml=inparm)
close(99, status="delete")

if (.not.first .or. any (other /= [3,2,1,1])) stop 1

other = 9

open(99, file="test.inp")
write(99, '(a)') nml_str2
rewind(99)
read(99,nml=inparm)
close(99, status="delete")

if (.not.first .or. any (other /= [3,2,9,9])) stop 2

ints = 66

open(99, file="test.inp", decimal='point')
write(99, '(a)') str
rewind(99)
read(99,*) ints
close(99, status="delete")

if (any (ints /= [1,2,3,4,5,66])) stop 3

ints = 77 

open(99, file="test.inp", decimal='comma')
write(99, '(a)') str2
rewind(99)
read(99,*) ints
close(99, status="delete")

if (any (ints /= [1,2,3,4,5,77])) stop 4
end
