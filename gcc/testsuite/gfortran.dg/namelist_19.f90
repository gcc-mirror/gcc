!{ dg-do run { target fd_truncate } }
! Test namelist error trapping.
! provided by Paul Thomas - pault@gcc.gnu.org

program namelist_19
  character*80 wrong, right
  
! "=" before any object name
  wrong = "&z = i = 1,2 /"
  right = "&z i = 1,2 /"
  call test_err(wrong, right)
  
! &* instead of &end for termination 
  wrong = "&z i = 1,2 &xxx"
  right = "&z i = 1,2 &end"
  call test_err(wrong, right)
  
! bad data 
  wrong = "&z i = 1,q /"
  right = "&z i = 1,2 /"
  call test_err(wrong, right)
  
! object name not matched 
  wrong = "&z j = 1,2 /"
  right = "&z i = 1,2 /"
  call test_err(wrong, right)

! derived type component for intrinsic type
  wrong = "&z i%j = 1,2 /"
  right = "&z i = 1,2 /"
  call test_err(wrong, right)

! step other than 1 for substring qualifier
  wrong = "&z ch(1:2:2) = 'a'/"
  right = "&z ch(1:2) = 'ab' /"
  call test_err(wrong, right)

! qualifier for scalar 
  wrong = "&z k(2) = 1 /"
  right = "&z k    = 1 /"
  call test_err(wrong, right)

! no '=' after object name 
  wrong = "&z i   1,2 /"
  right = "&z i = 1,2 /"
  call test_err(wrong, right)

! repeat count too large 
  wrong = "&z i = 3*2 /"
  right = "&z i = 2*2 /"
  call test_err(wrong, right)

! too much data 
  wrong = "&z i = 1 2 3 /"
  right = "&z i = 1 2 /"
  call test_err(wrong, right)

! no '=' after object name 
  wrong = "&z i   1,2 /"
  right = "&z i = 1,2 /"
  call test_err(wrong, right)

! bad number of index fields
  wrong = "&z i(1,2) = 1 /"
  right = "&z i(1)   = 1 /"
  call test_err(wrong, right)

! bad character in index field 
  wrong = "&z i(x) = 1 /"
  right = "&z i(1) = 1 /"
  call test_err(wrong, right)

! null index field 
  wrong = "&z i( ) = 1 /"
  right = "&z i(1) = 1 /"
  call test_err(wrong, right)

! null index field 
  wrong = "&z i(1::)   = 1 2/"
  right = "&z i(1:2:1) = 1 2 /"
  call test_err(wrong, right)

! null index field 
  wrong = "&z i(1:2:)  = 1 2/"
  right = "&z i(1:2:1) = 1 2 /"
  call test_err(wrong, right)

! index out of range 
  wrong = "&z i(10) = 1 /"
  right = "&z i(1)  = 1 /"
  call test_err(wrong, right)

! index out of range 
  wrong = "&z i(0:1) = 1 /"
  right = "&z i(1:1) = 1 /"
  call test_err(wrong, right)

! bad range
  wrong = "&z i(1:2:-1) = 1 2 /"
  right = "&z i(1:2: 1) = 1 2 /"
  call test_err(wrong, right)

! bad range
  wrong = "&z i(2:1: 1) = 1 2 /"
  right = "&z i(2:1:-1) = 1 2 /"
  call test_err(wrong, right)

contains
  subroutine test_err(wrong, right)
    character*80 wrong, right
    integer            :: i(2) = (/0, 0/)
    integer            :: k =0
    character*2        :: ch = "  "
    namelist /z/ i, k, ch

! Check that wrong namelist input gives an error

    open (10, status = "scratch")
    write (10, '(A)') wrong
    rewind (10)
    read (10, z, iostat = ier)
    close(10)
    if (ier == 0) call abort ()

! Check that right namelist input gives no error

    open (10, status = "scratch")
    write (10, '(A)') right
    rewind (10)
    read (10, z, iostat = ier)
    close(10)
    if (ier /= 0) call abort ()
  end subroutine test_err
  
end program namelist_19
