! { dg-do run }
! Tests the fix for pr28167, in which character array constructors
! with an implied do loop would cause an ICE, when used as actual
! arguments.
!
! Based on the testscase by Harald Anlauf  <anlauf@gmx.de>
!
  character(4), dimension(4) :: c1, c2
  integer m
  m = 4
! Test the original problem
  call foo ((/( 'abcd',i=1,m )/), c2)
  if (any(c2(:) .ne. (/'abcd','abcd', &
                            'abcd','abcd'/))) call abort ()

! Now get a bit smarter
  call foo ((/"abcd", "efgh", "ijkl", "mnop"/), c1) ! worked previously
  call foo ((/(c1(i), i = m,1,-1)/), c2)            ! was broken
  if (any(c2(4:1:-1) .ne. c1)) call abort ()

! gfc_todo: Not Implemented: complex character array constructors
  call foo ((/(c1(i)(i/2+1:i/2+2), i = 1,4)/), c2)  ! Ha! take that..!
  if (any (c2 .ne. (/"ab  ","fg  ","jk  ","op  "/))) call abort ()

! Check functions in the constructor
  call foo ((/(achar(64+i)//achar(68+i)//achar(72+i)// &
               achar(76+i),i=1,4 )/), c1)           ! was broken
  if (any (c1 .ne. (/"AEIM","BFJN","CGKO","DHLP"/))) call abort ()
contains
  subroutine foo (chr1, chr2)
    character(*), dimension(:) :: chr1, chr2
    chr2 = chr1
  end subroutine foo
end