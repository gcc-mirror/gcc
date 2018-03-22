! { dg-do run { target fd_truncate } }
! PR25039 This test checks that commas in input fields for formatted sequential
! reads are interpreted as the read completion.  If no comma is encountered the
! normal field width determines the end of the read.  The test case also checks
! that default blanks are interpreted as NULL in numerics.
! Test case derived from sample provided in PR by Iwan Kawrakow.
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
!
      program pr25039
      implicit none
      integer :: i1, i2, i3
      character(10) :: a1
      open(10, status="scratch")
      write(10,'(a)') "1, 235"
      rewind(10)
      read(10,'(3i2)') i1,i2,i3
      if(i1.ne.1) STOP 1
      if(i2.ne.2) STOP 2
      if(i3.ne.35) STOP 3
      rewind(10)
!     Make sure commas are read in character strings.      
      write(10,'(a)') "1234,6789,"
      rewind(10)
      read(10,'(a10)') a1
      if(a1.ne."1234,6789,") STOP 4
      end
