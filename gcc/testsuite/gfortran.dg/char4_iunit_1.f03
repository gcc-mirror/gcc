! { dg-do run }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } { "*" } { "" } }
! PR37077 Implement Internal Unit I/O for character KIND=4
! Test case prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program char4_iunit_1
  implicit none
  character(kind=4,len=44) :: string
  integer(kind=4) :: i,j
  real(kind=4) :: inf, nan, large

  large = huge(large)
  inf = 2 * large
  nan = 0
  nan = nan / nan

  string = 4_"123456789x"
  write(string,'(a11)') 4_"abcdefg"
  if (string .ne. 4_"    abcdefg                               ") call abort
  write(string,*) 12345
  if (string .ne. 4_"       12345                              ") call abort
  write(string, '(i6,5x,i8,a5)') 78932, 123456, "abc"
  if (string .ne. 4_" 78932       123456  abc                  ") call abort
  write(string, *) .true., .false. , .true.
  if (string .ne. 4_" T F T                                    ") call abort
  write(string, *) 1.2345e-06, 4.2846e+10_8
  if (string .ne. 4_"   1.23450002E-06   42846000000.000000      ") call abort
  write(string, *) nan, inf
  if (string .ne. 4_"              NaN         Infinity    ") call abort
  write(string, '(10x,f3.1,3x,f9.1)') nan, inf
  if (string .ne. 4_"          NaN    Infinity             ") call abort
  write(string, *) (1.2, 3.4 )
  if (string .ne. 4_" (  1.20000005    ,  3.40000010    )  ") call abort
end program char4_iunit_1
