! { dg-do run }
! The compiler_options() function is dependent on the
! command line options and thus incompatible with -fcompare-debug.
! { dg-skip-if "-fcompare-debug incompatible test" { *-*-* } { "-fcompare-debug" } { "" } } */
!
! Test the fix for PR92785, where the array passed to 'write scalar' was not
! normalised to LBOUND = 1.
!
! Contributed by  <urbanjost@comcast.net>
!
   program tst
      use iso_fortran_env, only : compiler_version, compiler_options
      implicit none
      integer :: i
      integer :: ibad=0
      integer :: iarr(10) = [(i*10, i = 1,size (iarr))]
      character(len=:), allocatable :: line
      character(len=*), parameter :: expected = '10 20 30 40 50 60 70 80 90 100'
      character(len=*), parameter :: expected_minus = '-10 -20 -30 -40 -50 -60 -70 -80 -90 -100'
      print '(4a)', &
         'This file was compiled by ', compiler_version(), &
         ' using the options ',        compiler_options()
      call write_row ('iarr                   ', iarr)                    ! pass in the array, OK
      call write_row ('iarr+0                 ', iarr+0)                  ! pass in an expression, NOT OK
      call write_row ('-iarr                  ', -iarr)                   ! pass in an expression, NOT OK
      call write_row ('iarr(::1)              ', iarr(::1))               ! pass in the array, OK
      call write_row ('[iarr(::1)]            ', [iarr(::1)])             ! pass in compound constructor, NOT OK
      call write_row ('[(i*10,i=1,size(iarr))]', [(i*10,i=1,size(iarr))]) ! pass in constructor, OK
      call write_row ('10*[(i,i=1,size(iarr))]', 10*[(i,i=1,size(iarr))]) ! pass in constructor, OK
      if (ibad .gt. 0) stop 1
   contains
      subroutine write_scalar (g1)
         class(*) :: g1
         character(len = 20) :: word
         select type(g1)
          type is (integer)
            write (word, '(i0)') g1
            line = line // trim( word) // ' '
         end select
      end subroutine write_scalar
      subroutine write_row (string,array)
         character(len = *) :: string
         class(*) :: array(:)
         integer  :: i
         line = ''
         do i = 1, size (array)
            call write_scalar (array(i))
         enddo
         if (expected .eq. line) then
            write (*, *) string, ':GOOD'
         else if (expected_minus .eq. line) then
            write (*, *) string, ':GOOD'
         else
            write (*, *) string, ':BAD. EXPECTED [', expected, '] got [', trim (line),']'
            ibad = ibad + 1
         endif
      end subroutine write_row
   end program tst
